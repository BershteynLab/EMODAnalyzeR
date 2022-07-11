
calculate.cost <- function(simulation.data,
                           cost.treatment_initiation,
                           cost.treatment_annual_2025through2027,
                           cost.treatment_annual_2028onward,
                           name.treatment_population_column,
                           name.treatment_initiation_column,
                           cost.art_annual = 257
                           ) {
  simulation.data$Received_Treatment_Scaled <- simulation.data[,name.treatment_initiation_column] * simulation.data$pop_scaling_factor
  simulation.data$On_Treatment_Scaled <- simulation.data[,name.treatment_population_column] * simulation.data$pop_scaling_factor
  simulation.data.cost <- simulation.data %>%
    dplyr::group_by(Year, Age, sim.id ) %>%
    dplyr::summarize(On_Treatment = sum(On_Treatment_Scaled),
              Treatment_Initiated = sum(Received_Treatment_Scaled),
              ART_Averted = sum(ART_Averted * pop_scaling_factor)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( Year_Integer = floor((simulation.data$Year-0.5))) %>%
    dplyr::group_by(Year_Integer, Age, sim.id) %>%
    dplyr::summarize(ART_Averted = mean(ART_Averted), On_Treatment = sum(Treatment_Initiated),
              Treatment_Initiated = sum(Treatment_Initiated))

  Treatment.cost.through.2027 <- simulation.data.cost %>%
    dplyr::mutate( young.on.Treatment = if_else( (Age >= 15) & (Age < 40), On_Treatment, 0)) %>%
    dplyr::filter((Year_Integer < 2028) & (Year_Integer >= 2025 )) %>%
    dplyr::mutate( cost =
              Treatment_Initiated * ( cost.treatment_initiation ) +
              On_Treatment * cost.treatment_annual_2025through2027) %>%
    dplyr::mutate( cost_art_averted = cost - ( ART_Averted * cost.art_annual) ) %>%
    dplyr::group_by( sim.id, Year_Integer ) %>%
    dplyr::summarize(cost = sum(cost),
              cost_art_averted = sum(cost_art_averted),
              PY_On_Treatment = sum(On_Treatment),
              young.on.Treatment = sum(young.on.Treatment))


  Treatment.cost.2028.onwards <- simulation.data.cost %>%
    dplyr::mutate( young.on.Treatment = if_else( (Age >= 15) & (Age < 40), On_Treatment, 0)) %>%
    dplyr::filter((Year_Integer >= 2028)) %>%
    dplyr::mutate( cost =
              Treatment_Initiated * cost.treatment_initiation +
              On_Treatment * cost.treatment_annual_2028onward ) %>%
    dplyr::mutate( cost_art_averted = cost - ( ART_Averted * cost.art_annual) ) %>%
    dplyr::group_by( sim.id, Year_Integer ) %>%
    dplyr::summarize(cost = sum(cost),
              cost_art_averted = sum(cost_art_averted),
              PY_On_Treatment = sum(On_Treatment),
              young.on.Treatment = sum(young.on.Treatment))

  Treatment.cost <- rbind( Treatment.cost.through.2027, Treatment.cost.2028.onwards )
  Treatment.cost %>%
    dplyr::rename(year = Year_Integer)
}


calculate.infections.averted <- function(data.intervention, data.nointervention, start_year = 2025, end_year = 2055) {
  data.intervention$Year_Integer <- floor((data.intervention$Year-0.5))
  data.intervention$infections_averted <- (data.nointervention$Newly.Infected - data.intervention$Newly.Infected) * data.intervention$pop_scaling_factor
  data.intervention %>%
    dplyr::group_by(Year_Integer) %>%
    dplyr::summarise(infections_averted = sum(infections_averted)) %>%
    dplyr::rename( year = Year_Integer )
}

icer.calculate <- function(data.intervention,
                           data.nointervention,
                           discount_rate,
                           cost.treatment_initiation,
                           cost.treatment_annual_2025through2027,
                           cost.treatment_annual_2028onward,
                           end_year,
                           name.treatment_population_column,
                           name.treatment_initiation_column,
                           life_expectancy,
                           additional_DALY_per_PY_On_Treatment = 0,
                           use_condom = FALSE) {

  Treatment.sims = unique(data.intervention$sim.id)
  noTreatment.sims = unique(data.nointervention$sim.id)
  icers_with_art = rep(NA, length(Treatment.sims))
  icers_without_art = rep(NA, length(Treatment.sims))
  cost_per_infection_averted = rep(NA, length(Treatment.sims))
  averted = rep(NA, length(Treatment.sims))
  MMYLLaverted = rep(NA, length(Treatment.sims))
  cost = rep(NA, length(Treatment.sims))
  infections = rep(NA, length(Treatment.sims))
  PYonTreatment = rep(NA, length(Treatment.sims))
  Avert1DALY = rep(NA, length(Treatment.sims))
  Avert1Infection = rep(NA, length(Treatment.sims))

  for (iSim in seq(1,length(Treatment.sims))) {
    this_data.intervention <- data.intervention[data.intervention$sim.id == Treatment.sims[iSim],]
    this_data.nointervention <- data.nointervention[data.nointervention$sim.id == noTreatment.sims[iSim],]
    this_data.intervention$ART_Averted <- this_data.nointervention$On_ART - this_data.intervention$On_ART
    this_cost = calculate.cost(this_data.intervention,
                               cost.treatment_initiation,
                               cost.treatment_annual_2025through2027,
                               cost.treatment_annual_2028onward,
                               name.treatment_population_column,
                               name.treatment_initiation_column)
    this_daly.Treatment = calculate.DALY(this_data.intervention, discount_percent = discount_rate, art_weight = 0.1, life_expectancy = life_expectancy)
    this_daly.noTreatment = calculate.DALY(this_data.nointervention, discount_percent = discount_rate, art_weight = 0.1, life_expectancy = life_expectancy)
    this_daly.Treatment$averted = this_daly.noTreatment$daly_future_discounted - this_daly.Treatment$daly_future_discounted
    if (use_condom) {
      this_daly.Treatment$averted <- this_daly.Treatment$averted * (1 - (0.78 / 0.90))
    }
    daly_and_cost = left_join(this_cost, this_daly.Treatment, by = "year", suffix= c('cost','daly')) %>% filter(year < end_year)
    daly_and_cost$cost_discounted = daly_and_cost$cost * daly_and_cost$discount_factor
    daly_and_cost$MMLLYAverted = daly_and_cost$young.on.Treatment * daly_and_cost$discount_factor * additional_DALY_per_PY_On_Treatment
    daly_and_cost$averted <- daly_and_cost$averted + daly_and_cost$MMLLYAverted
    daly_and_cost$cost_discounted_ART = daly_and_cost$cost_art_averted * daly_and_cost$discount_factor
    cost_art_discounted = sum(daly_and_cost$cost_discounted_ART)
    cost_raw = sum(daly_and_cost$cost_discounted)
    icers_with_art[iSim] = cost_art_discounted / sum(daly_and_cost$averted)
    icers_without_art[iSim] = cost_raw / sum(daly_and_cost$averted)
    infections_averted = calculate.infections.averted(this_data.intervention, this_data.nointervention, end_year = end_year)
    infection_and_daly = left_join(infections_averted, this_daly.Treatment, by = "year", suffix= c('infections','daly'))
    infection_and_daly$infection_discounted = infection_and_daly$infections_averted * infection_and_daly$discount_factor
    cost_per_infection_averted[iSim] = cost_art_discounted / sum(infection_and_daly$infection_discounted)
    PYonTreatment[iSim] = sum(daly_and_cost$PY_On_Treatment * daly_and_cost$discount_factor )
    Avert1DALY[iSim] = PYonTreatment[iSim] / sum(daly_and_cost$averted)
    Avert1Infection[iSim] = PYonTreatment[iSim] / sum(infection_and_daly$infection_discounted)
    MMYLLaverted[iSim] = sum(daly_and_cost$MMLLYAverted)
    averted[iSim] = sum(daly_and_cost$averted)
    cost[iSim] = cost_art_discounted
    infections[iSim] = sum(infection_and_daly$infection_discounted)
  }

  tibble(icers_with_art,icers_without_art,cost_per_infection_averted,PYonTreatment,Avert1DALY,Avert1Infection, averted, MMYLLaverted, cost, infections)

}


icer.run.comparison <- function (data.intervention,
                            data.nointervention,
                            discount_rate,
                            experiment_name,
                            cost.treatment_initiation = 21.39 / 3,
                            cost.treatment_annual_2025through2027 = 131.46,
                            cost.treatment_annual_2028onward = 110.61,
                            name.treatment_population_column = "Received_DPP",
                            name.treatment_initiation_column = "Received_DPP",
                            start_year = 2025,
                            end_year = 2055,
                            additional_DALY_per_PY_On_Treatment = 0,
                            use_condom = FALSE,
                            life_expectancy = 66 ) {

  icer.data = icer.calculate(data.intervention ,
                             data.nointervention ,
                             discount_rate,
                             cost.treatment_initiation,
                             cost.treatment_annual_2025through2027,
                             cost.treatment_annual_2028onward,
                             end_year,
                             name.treatment_population_column,
                             name.treatment_initiation_column,
                             life_expectancy,
                             additional_DALY_per_PY_On_Treatment, use_condom )

  summary_fun <- function (.data) {
    .data %>% dplyr::summarize(icer = sum(cost)/sum(averted),
                        cost_per_infection_averted = sum(cost) / sum(infections),
                        PYonTreatment_per_infection_averted = sum(PYonTreatment) / sum(infections),
                        PYonTreatment_per_DALY_averted = sum(PYonTreatment) / sum(averted),
                        averted = mean(averted),
                        mmyyllaverted = mean(MMYLLaverted),
                        PYonTreatment = mean(PYonTreatment))
  }
  bt_resamples <- bootstraps(icer.data, times = 500)
  resamples <- map_dfr(bt_resamples$splits, function(.data) {.data %>% analysis() %>% summary_fun()} )
  icer.summary <- rbind( icer.data %>% summary_fun %>% mutate(aggregation = "Mean"),
                         resamples %>% dplyr::summarize_all(function(data) {quantile(data, 0.025)}) %>% mutate(aggregation = "Lower Bound (95% CI)"),
                         resamples %>% dplyr::summarize_all(function(data) {quantile(data, 0.975)}) %>% mutate(aggregation = "Upper Bound (95% CI)"))
  icer.summary$comparison = experiment_name
  icer.summary
}

