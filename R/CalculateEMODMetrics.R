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

calculate.scaled_data <- function(data, reference_year, reference_population, col2scale = 'Infected') {
  for_pop_scaling_factor <- aggregate(Population ~ Year + sim.id + scenario_name,
                                      subset(data,
                                             Year == reference_year), FUN=sum)

  for_pop_scaling_factor$pop_scaling_factor <- reference_population/for_pop_scaling_factor$Population
  data <- merge(data,for_pop_scaling_factor,by=c('sim.id', "scenario_name"),suffixes = c("",".y"))
  print(data)

  cbind(data[col2scale] * data$pop_scaling_factor,
        data[c('Year','Gender','sim.id','scenario_name')])

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
