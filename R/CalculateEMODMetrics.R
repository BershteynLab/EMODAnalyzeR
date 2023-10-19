
#' Calculate prevalence in an EMOD simulation
#'
#' Defaults to calculating HIV prevalence, but also may be used to calculate fraction of PLHIV who know their status, ART coverage, or other measures involving a numerator and denominator.
#' Prevalence = (numerator)/(denominator)
#'
#' @param data A tibble returned from read.simulation.results()
#' @param stratify_columns List of strings, names of stratifying columns included in the `group_by` function call before aggregation. The list should include sim.id (or sim.ix) as an argument to calculate the prevalence for each individual simulation run.
#' @param numerator String, name of prevalence numerator
#' @param denominator String, name of prevalence denominator
#'
calculate.prevalence <- function(data, stratify_columns = c("Year", "Gender", "sim.id"), numerator = 'Infected', denominator = 'Population'){
  data['num'] = data[numerator]
  data['den'] = data[denominator]
  data <- data %>%
    dplyr::group_by_at(stratify_columns) %>%
    dplyr::summarize_at(c('num', 'den'), sum, na.rm = T) %>%
    ungroup() %>%
    dplyr::mutate(Prevalence = case_when(den == 0 ~ 0,
                                         den > 0 ~ num/den))

  colnames(data) <- c(stratify_columns, numerator, denominator, "Prevalence")

  return(data)
}

#' Calculate incidence of an EMOD simulation
#'
#' @param data A tibble returned from read.simulation.results(..., stratify_columns = c("Year", "Gender"), aggregate_columns = c("Population","Newly.Infected", "Infected")).
#' More aggregate columns can be used, but more stratify columns will be ignored
#' @param gender.breakdown - Boolean which controls whether to disaggregate the data by gender. Defaults to TRUE.
#' @param debug - Boolean which controls whether function returns intermediate results. Defaults to TRUE.
#' @return A tibble with columns incidence and Year
calculate.incidence <- function(data,gender.breakdown = TRUE, debug = FALSE) {
  # Group rows together by year
  data$Year_Integer <- floor((data$Year-0.5))

  if (gender.breakdown == TRUE){
    # Aggregate number of new infections each year, broken down by Year_Integer and Gender
    trajectories_IR.1a <- aggregate(Newly.Infected ~ Year_Integer+Gender+sim.id+scenario_name, data=data,FUN=sum)
    if (debug) print(head(trajectories_IR.1a, 10)) # nocov
    #Make the denominator as HIV-negative individuals
    trajectories_IR.2 <- aggregate(Population - Infected ~ Year+Gender+sim.id+scenario_name, data=data, FUN=sum)
    if (debug) print(head(trajectories_IR.2,10)) # nocov
    trajectories_IR.2$Year_Integer <- floor(trajectories_IR.2$Year-0.5)
    #remove second instance of duplicate rows
    trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year_Integer","Gender","sim.id","scenario_name")]),]
    trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]
    trajectories_IRoverall <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year_Integer","Gender","sim.id","scenario_name"))
  } else {
    # Aggregate number of new infections each year, broken down by Year_Integer but not by Gender
    trajectories_IR.1a <- aggregate(Newly.Infected ~ Year_Integer+sim.id+scenario_name, data=data,FUN=sum)
    if (debug) print(head(trajectories_IR.1a, 10)) # nocov
    #Make the denominator as HIV-negative individuals
    trajectories_IR.2 <- aggregate(Population - Infected ~ Year+sim.id+scenario_name, data=data, FUN=sum)
    if (debug) print(head(trajectories_IR.2,10)) # nocov
    trajectories_IR.2$Year_Integer <- floor(trajectories_IR.2$Year-0.5)
    #remove second instance of duplicate rows
    trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year_Integer","sim.id","scenario_name")]),]
    trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]
    trajectories_IRoverall <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year_Integer","sim.id","scenario_name"))
  }

  trajectories_IRoverall$incidence <- trajectories_IRoverall$Newly.Infected / (trajectories_IRoverall$Population)
  trajectories_IRoverall %>% dplyr::rename(Year = Year_Integer)

}


#' Calculate number of tests performed in an EMOD simulation
#'
#' @param data A tibble returned from read.simulation.results(..., stratify_columns = c("Year", "Gender"), aggregate_columns = c("Population","Newly.Infected", "Infected")).
#' More aggregate columns can be used, but more stratify columns will be ignored
#' @param gender.breakdown - Boolean which controls whether to disaggregate the data by gender. Defaults to TRUE.
#' @param debug - Boolean which controls whether function returns intermediate results. Defaults to TRUE.
#' @return A tibble with columns Year; Number of Negative Tests performed; Number of Positive Tests performed; Total Tests performed; Proportion of Positive Tests performed.
calculate.tests.performed <- function(data, gender.breakdown = TRUE, debug = FALSE) {
  # Group rows together by year
  data$Year_Integer <- floor((data$Year-0.5))

  if (gender.breakdown == TRUE){
    # Aggregate number of positive and negative tests, broken down by Year_Integer and Gender
    test.trajectories <- aggregate(cbind(Newly.Tested.Positive, Newly.Tested.Negative) ~ Year_Integer+Gender+sim.id+scenario_name, data=data,FUN=sum)
    if (debug) print(head(test.trajectories, 10)) # nocov
  } else {
    # Aggregate number of positive and negative tests, broken down by Year_Integer but not by Gender
    test.trajectories <- aggregate(cbind(Newly.Tested.Positive, Newly.Tested.Negative) ~ Year_Integer+sim.id+scenario_name, data=data,FUN=sum)
    if (debug) print(head(test.trajectories, 10)) # nocov
  }
  # Calculate Total Tests and Proportion of Positive Tests
  test.trajectories %>%
    dplyr::rename(Year = Year_Integer) %>%
    mutate(Total.Tests = Newly.Tested.Negative + Newly.Tested.Positive) %>%
    mutate(Proportion.Positive.Tests = case_when(Total.Tests == 0 ~ 0,
                                                 Total.Tests > 0 ~ Newly.Tested.Positive/Total.Tests))
}


#' Calculate the scaling factor for a population run on EMOD. Typically when we run EMOD, we scale down our population for the sake of
#' computation costs. In order to get correct counts of different statuses (i.e., number of people who died from HIV), we need to scale our data.
#' This function will not scale the data, rather it will add a scaling-factor column to the data that you can multiply any column of interest by.
#'
#' @param data A tibble returned from read.simulation.results(..., stratify_columns = c("Year"), aggregate_columns = c("Population")).
#' More aggregate and stratify columns can be used
#' @param reference_year The year that you want to calibrate the total population to
#' @param reference_population the actual population of the area being studied at the time of "reference_year"
#' @return A tibble with all original columns found in "data", plus a column for pop_scaling_factor
calculate.pop_scaling_factor <- function(data, reference_year, reference_population, age_min_inclusive=0, age_max_inclusive = 200) {
  data.within.age <- data %>%
    filter((Age >= age_min_inclusive) & (Age <= age_max_inclusive))
  for_pop_scaling_factor <- aggregate(Population ~ Year + sim.id + scenario_name,
                                      subset(data.within.age,
                                             Year == reference_year), FUN=sum)

  for_pop_scaling_factor$pop_scaling_factor <- reference_population/for_pop_scaling_factor$Population
  data <- dplyr::inner_join(data,
                            for_pop_scaling_factor
                            %>% select(pop_scaling_factor, sim.id, scenario_name),
                            by=c('sim.id', "scenario_name"), suffix = c("",".y"))
  data

}

#' Calculate the DALY population run on EMOD. DALY stands for Disability Adjusted Life Years. It is a measure of years lost
#' due to a disease - in this case, HIV. It is adjusted for the disability of living with the disease. By default, living
#' with untreated HIV has a weight of 0.3 (or a year with HIV is worth 70% of a healthy year). Living under ART treatment is given
#' a weight of 0.1. We also discount future years of DALY. By default, each future year is discounted a compounding 3%. For example,
#' by the 10th year, we the DALY will be multiplied by 0.97^10.
#'
#' @param data A tibble returned from calculate.pop_scaling_factor, which was passed data from
#'  read.simulation.results(..., stratify_columns = c("Year", "Age"),
#'                               aggregate_columns = c("Died_from_HIV", "Infected", "On_ART")).
#' @param infected_weight The disability weight for being infected with HIV, but without treatment (default 0.274)
#' @param art_weight The disability weight for being infected with HIV, but on ART (default 0.1)
#' @param discount_start_year The year in which the DALY starts becoming reduced for being in the future
#' @param discount_percent The compounding percent to reduce DALY each year in the future
#' @return A tibble with columns year, daly, and daly_future_discounted
calculate.DALY <- function(data,   infected_weight = 0.274, art_weight = 0.078, discount_start_year = 2023, discount_percent = 0.03, life_expectancy = 80) {
  data_by_age_year <- data %>%
                      dplyr::group_by(Year, Age, sim.id, scenario_name) %>%
                      dplyr::summarise(Died_from_HIV = sum(Died_from_HIV),
                                       Infected = sum(Infected),
                                       On_ART = sum(On_ART),
                                       pop_scaling_factor = mean(pop_scaling_factor))


  data_by_age_year$Year_Integer <- floor((data_by_age_year$Year-0.5))


  m2 <- data_by_age_year %>%
        dplyr::group_by(Year_Integer, Age, sim.id, scenario_name) %>%
        dplyr::summarise(Died_from_HIV = sum(Died_from_HIV),
                         Infected = mean(Infected),
                         On_ART = mean(On_ART),
                         pop_scaling_factor = mean(pop_scaling_factor))

  m2 <- m2 %>%
        mutate(On_ART_calib = On_ART*pop_scaling_factor,
        Infected_calib = Infected*pop_scaling_factor,
        Died_from_HIV_calib = Died_from_HIV*pop_scaling_factor,
        Infected_off_ART_calib = (Infected - On_ART)*pop_scaling_factor)

  m2$Age <- ifelse(m2$Age >life_expectancy, life_expectancy, m2$Age)

  m3 <- m2 %>%
        dplyr::group_by(Year_Integer, Age, scenario_name) %>%
        dplyr::select(Year_Integer, Age, scenario_name, Died_from_HIV_calib, Infected_off_ART_calib, On_ART_calib) %>%
        dplyr::summarise_all(list(median=median))

  disability.tibble <- m3 %>%
                        dplyr::group_by(Year_Integer) %>%
                        dplyr::summarize(infected_untreated = sum(Infected_off_ART_calib_median),
                                         on_art = sum(On_ART_calib_median)) %>%
                        dplyr::rename(year = Year_Integer)

  m4 <- m3 %>% mutate(year_applied = Year_Integer - (Age - (life_expectancy + 1)))

  daly.tibble <- tibble(year = seq(min(m4$Year_Integer), max(m4$Year_Integer)))

  yll <- daly.tibble %>% rowwise() %>%
            do( yll = sum(  m4[(m4$Year_Integer <= .$year) &
                                 (m4$year_applied > .$year),'Died_from_HIV_calib_median'] ),
                            year = .$year[[1]] ) %>%
            unnest()

  daly = left_join(yll, disability.tibble, by="year") %>%
         replace(is.na(.), 0)

  daly <- daly %>% mutate(daly = infected_untreated*infected_weight + on_art*art_weight + yll)
  daly <- daly %>% mutate(discount_factor = (1 - discount_percent)^(year - discount_start_year) )
  daly[daly$year < discount_start_year,'discount_factor'] <- 1.0
  daly <- daly %>% mutate(daly_future_discounted = daly*discount_factor )

  return (daly)

}


#' Calculate the 95 percent confidence interval of an estimate given 2 sigma. This is used to
#' calculate confidence intervals from EMOD calibration data that comes from a Gaussian distribution
#'
#' @param mean_vector A vector of the estimated value of a measurement
#' @param two_sigma_vector A vector of the two standard deviations from the mean of a measurement
#' @return A data.frame with columns lb and ub corresponding to upper and lower bounds (95% CI)
calculate.bounds.two_sigma <- function(mean_vector, two_sigma_vector) {
  one.96_sigma = ( two_sigma_vector / 2.0 ) * 1.96
  lb <- mean_vector - one.96_sigma
  ub <- mean_vector + one.96_sigma
  names(lb) <- c('lb')
  names(ub) <- c('ub')
  data.frame(lb = lb, ub = ub)
}

#' Calculate the 95 percent confidence interval of an estimate given effective counts. This is used to
#' calculate confidence intervals from EMOD calibration data that comes from a Beta distribution
#'
#' @param mean_vector A vector of the estimated value of a measurement
#' @param effective_counts A vector of the effective number of samples from which the measurement was made
#' @return A tibble with columns lb and ub corresponding to upper and lower bounds (95% CI)
calculate.bounds.effective_count <- function (mean_vectors, effective_counts) {
  x <- as.vector(mean_vectors * effective_counts)
  alpha <- x + 1
  beta <- effective_counts - x + 1
  names(alpha) <- c('alpha')
  names(beta) <- c('beta')
  print(tibble(alpha, beta))
  tibble(alpha, beta) %>%
    mutate(lb = qbeta(0.025, alpha, beta)) %>%
    mutate(ub = qbeta(0.975, alpha, beta)) %>%
    select(lb, ub)
}
