calculate.incidence <- function(data) {
  data$Year_Integer <- floor((data$Year-0.5))

  trajectories_IR.1a <- aggregate(Newly.Infected ~ Year_Integer+Gender+sim.id+scenario_name, data=data,FUN=sum) #sum number of new infections in each year
  print(head(trajectories_IR.1a, 10))

  #Make the denominator as HIV-negative individuals
  trajectories_IR.2 <- aggregate(Population - Infected ~ Year+Gender+sim.id+scenario_name, data=data, FUN=sum)
  head(trajectories_IR.2,10)

  trajectories_IR.2$Year_Integer <- floor(trajectories_IR.2$Year-0.5)

  trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year_Integer","Gender","sim.id","scenario_name")]),] #remove second instance of duplicate rows

  trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]

  trajectories_IRoverall <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year_Integer","Gender","sim.id","scenario_name"))

  trajectories_IRoverall$incidence <- trajectories_IRoverall$Newly.Infected / (trajectories_IRoverall$Population-(trajectories_IRoverall$Newly.Infected/2))
  trajectories_IRoverall %>% dplyr::rename(Year = Year_Integer)

}

calculate.pop_scaling_factor <- function(data, reference_year, reference_population) {
  for_pop_scaling_factor <- aggregate(Population ~ Year + sim.id + scenario_name,
                                      subset(data,
                                             Year == reference_year), FUN=sum)

  for_pop_scaling_factor$pop_scaling_factor <- reference_population/for_pop_scaling_factor$Population
  data <- merge(data,
                for_pop_scaling_factor
                %>% select(pop_scaling_factor, sim.id, scenario_name),
                by=c('sim.id', "scenario_name"),suffixes = c("",".y"))
  data

}

calculate.bounds.two_sigma <- function(mean_vector, two_sigma_vector) {
  one.96_sigma = ( two_sigma_vector / 2.0 ) * 1.96
  lb <- mean_vector - one.96_sigma
  ub <- mean_vector + one.96_sigma
  names(lb) <- c('lb')
  names(ub) <- c('ub')
  data.frame(lb = lb, ub = ub)
}

calculate.bounds.effective_count <- function (mean_vectors, effective_counts) {
  x <- as.vector(mean_vectors * effective_counts)
  alpha <- x + 1
  beta <- effective_counts - x + 1
  names(alpha) <- c('alpha_1')
  names(beta) <- c('beta_1')
  tibble(alpha, beta) %>%
    mutate(lb = qbeta(0.025, alpha_1, beta_1)) %>%
    mutate(ub = qbeta(0.975, alpha_1, beta_1)) %>%
    select(lb, ub)
}


calculate.DALY <- function(scenario_allyr.all.age,   infected_weight = 0.3, art_weight = 0.1) {

  scenario_allyr.all.age$Year_Integer <- floor((scenario_allyr.all.age$Year-0.5))


  m2 <- scenario_allyr.all.age %>%
        dplyr::group_by(Year_Integer, Age, sim.id, scenario_name) %>%
        dplyr::summarise(Died_from_HIV = sum(Died_from_HIV),
                         Infected = sum(Infected),
                         On_ART = sum(On_ART),
                         pop_scaling_factor = mean(pop_scaling_factor))

  m2 <- m2 %>%
        dplyr::group_by(Year_Integer, Age, sim.id, scenario_name)%>%
        mutate(On_ART_calib = On_ART*pop_scaling_factor,
        Infected_calib = Infected*pop_scaling_factor,
        Died_from_HIV_calib = Died_from_HIV*pop_scaling_factor,
        Infected_off_ART_calib = (Infected - On_ART)*pop_scaling_factor)

  m2$Age <- ifelse(m2$Age >80, 80, m2$Age)

  m3 <- m2 %>%
        select(Year_Integer, Age, sim.id, scenario_name, Died_from_HIV_calib, Infected_off_ART_calib, On_ART_calib) %>%
        dplyr::group_by(Year_Integer, Age, scenario_name) %>%
        dplyr::summarise_all(list(median=median))

  disability.tibble <- m3 %>%
                        dplyr::group_by(Year_Integer) %>%
                        dplyr::summarize(infections = sum(Infected_off_ART_calib_median),
                                                    on_art = sum(On_ART_calib_median)) %>%
                        dplyr::rename(year = Year_Integer)

  m4 <- m3 %>% mutate(
                      year_applied = Year_Integer - (Age - 81))

  daly.tibble = tibble(year = seq(min(m4$Year_Integer), max(m4$Year_Integer)))

  yll = daly.tibble %>% rowwise() %>%
        do( yll = sum(
          m4[(m4$Year_Integer < .$year) & (m4$year_applied > .$year),]$Died_from_HIV_calib_median ), year = .$year[[1]] ) %>%
        unnest()

  daly = left_join(yll, disability.tibble, on="year") %>%
         replace(is.na(.), 0)

  daly <- daly %>% mutate(daly = infections*infected_weight + on_art*art_weight + yll)
  return (daly)

}
