library(purrr)
library(rsample)
library(slurmR)
library(EMODAnalyzeR)

#run_report will output two csvs
run_report = function(root_path, cost_art, life_expectancy, pop_scale_param_inst) {

  emodplot.incidence = 
    function (data_baseline, intervention_df, date.start, date.end) 
    {
      data.incidence <- EMODAnalyzeR::calculate.incidence(data_baseline)
      y.lim.max <- min(max((data.incidence %>% filter(Year > date.start, Year < date.end))$incidence * 1.2), 1)
      intervention.incidence <- EMODAnalyzeR::calculate.incidence(intervention_df)
      all_data_incidence = rbind(data.incidence, intervention.incidence)
      p <- emodplot.by_gender(all_data_incidence, date.start, date.end, 
                              "incidence") + 
                              scale_y_continuous(labels = scales::percent_format(accuracy = .1), 
                                                 breaks = seq(0, y.lim.max, 0.005), limits = c(0, y.lim.max)) + 
                              ylab("HIV Incidence (%)")
      return(p)
    }
  
  dalys_by_sim = function(data_) {
    data_ %>%
    group_by(sim.id) %>% 
    group_map(
      function(data,group) {
        data %>% 
          mutate(sim.id = group[1,1]) %>% 
          EMODAnalyzeR::calculate.DALY(life_expectancy = life_expectancy) %>%
          mutate(sim.id = group$sim.id) 
      } 
    ) %>% bind_rows
  }
    
  get_infections_and_py = function (data) {
    infections = data %>% 
                  mutate(Year = floor(Year)) %>% 
                  group_by(Year, sim.id) %>% 
                  summarize(Infections=sum(Newly.Infected * pop_scaling_factor))
    py_on_treatment = data %>% filter(HasIntervention.LEN==1) %>% 
      group_by(Year, sim.id) %>% 
      summarize(Population=sum(Population * pop_scaling_factor)) %>% 
      mutate(Year = floor(Year)) %>% 
      group_by(Year, sim.id) %>% 
      summarize(py_on_treatment=mean(Population))
    py_on_oral_prep = data %>% filter(HasIntervention.Oral_PrEP==1) %>% 
      group_by(Year, sim.id) %>% 
      summarize(Population=sum(Population * pop_scaling_factor)) %>% 
      mutate(Year = floor(Year)) %>% 
      group_by(Year, sim.id) %>% 
      summarize(py_on_oral_prep=mean(Population))
    py_total = data %>% 
      group_by(Year, sim.id) %>% 
      summarize(Population=sum(Population * pop_scaling_factor)) %>% 
      mutate(Year = floor(Year)) %>% 
      group_by(Year, sim.id) %>% 
      summarize(py_total=mean(Population))
    inner_join(infections, py_on_treatment, by=c("Year","sim.id")) %>% 
      inner_join(py_total, by=c("Year","sim.id"))  %>%
      inner_join(py_on_oral_prep, by=c("Year","sim.id"))
  }
  # todo - offset year just like daly calculation
  infections_averted_over_time = function(baseline, intervention) {
    baseline_data = get_infections_and_py(baseline)
    intervention_data = get_infections_and_py(intervention)
    intervention_data$infections.averted = baseline_data$Infections - intervention_data$Infections
    intervention_data$infections.baseline = baseline_data$Infections
    results = intervention_data %>% 
              group_by(Year) %>%
              summarize(infections.averted = median(infections.averted),
                        py_on_treatment = median(py_on_treatment),
                        py_total = median(py_total),
                        baseline_infections = median(infections.baseline))
    results_time_on_treatment = results %>% filter(Year >= 2026, Year < 2036)
    results %>% ggplot() + 
      geom_point(aes(x=Year, y=infections.averted))
    data.frame(infections.averted=sum(results$infections.averted), 
               py_on_treatment=sum(results$py_on_treatment),
               percent_coverage=sum(results_time_on_treatment$py_on_treatment)/sum(results_time_on_treatment$py_total),
               percent_infections_averted = sum(results_time_on_treatment$infections.averted)/sum(results_time_on_treatment$baseline_infections))
    
  }
  
  infections_averted_by_simid = function(baseline, intervention) {
    baseline_data = get_infections_and_py(baseline)
    intervention_data = get_infections_and_py(intervention)
    intervention_data$infections.averted = baseline_data$Infections - intervention_data$Infections
    intervention_data$baseline_infections = baseline_data$Infections
    results = intervention_data %>% 
                filter(Year >= 2026, Year < 2036) %>%
                group_by(sim.id) %>%
                summarize(infections.averted=sum(infections.averted), 
                          py_on_treatment=sum(py_on_treatment),
                          percent_coverage=sum(py_on_treatment)/sum(py_total),
                          percent_infections_averted = sum(infections.averted)/sum(baseline_infections))
    results
  }
  
  calc_max_price = function(baseline, intervention, cost_art) {
    intervention = calculate.pop_scaling_factor(intervention, 
                                                pop_scale_param_inst$year, 
                                                reference_population = pop_scale_param_inst$population,
                                                age_max_inclusive = pop_scale_param_inst$age_max_inc,
                                                age_min_inclusive = pop_scale_param_inst$age_min_inc)
    py = get_infections_and_py(intervention)
    py_baseline = get_infections_and_py(baseline)
    py$oral_prep_averted = py_baseline$py_on_oral_prep - py$py_on_oral_prep
    dbs_intervention= dalys_by_sim(intervention)
    dbs_baseline = dalys_by_sim(baseline)
    dbs_intervention$dalys_averted = dbs_baseline$daly_future_discounted - dbs_intervention$daly_future_discounted
    dbs_intervention$on_art_averted = dbs_baseline$discount_factor * (dbs_baseline$on_art - dbs_intervention$on_art)
    py_and_dbs = inner_join(dbs_intervention %>% rename(Year = year), py, by=c("Year", "sim.id"))
    py_and_dbs = py_and_dbs %>% mutate(oral_prep_averted_discounted = oral_prep_averted * discount_factor)
    cost_delivery = (6 + 0.05 + 2.5) * 2 # cost of delivery + cost of syringe + COST OF test
    summary_fun <- function (.data) {
      .data %>% summarize(dalys_per_py = sum(dalys_averted) / sum(discounted_py), 
                          art_per_py = sum(on_art_averted) / sum(discounted_py),
                          dalys_per_py_minus_prep = sum(dalys_averted) / sum(discounted_py - oral_prep_averted_discounted),
                          art_per_py_minus_prep = sum(on_art_averted) / sum(discounted_py - oral_prep_averted_discounted),
                          py_on_oral_averted_per_py_on_len = sum(oral_prep_averted_discounted) / sum(discounted_py)) %>%
                mutate(max_cost_without_art = 500*dalys_per_py) %>% 
                mutate(max_cost_with_art = max_cost_without_art + cost_art*art_per_py) %>%
                mutate(demand_generation = .1 * max_cost_with_art) %>%
                mutate(max_cost_with_art_minus_dg = max_cost_with_art - demand_generation) %>%
                mutate(max_cost_with_art_minus_dg_delivery = max_cost_with_art_minus_dg - cost_delivery) %>%
                mutate(max_cost_per_dose = max_cost_with_art_minus_dg_delivery / 2) %>%
                mutate(wastage_per_dose = 0.05*max_cost_per_dose) %>%
                mutate(max_cost_per_dose_minus_wastage = max_cost_per_dose - wastage_per_dose)
    }
    bootstraps = 
      py_and_dbs %>% 
        mutate(discounted_py = py_on_treatment * discount_factor) %>% group_by(sim.id) %>%
        summarize (dalys_averted = sum(dalys_averted),
                   discounted_py = sum(discounted_py),
                   on_art_averted = sum(on_art_averted),
                   undiscounted_py = sum(py_on_treatment),
                   oral_prep_averted_discounted = sum(oral_prep_averted_discounted)) %>% 
        bootstraps( times = 500 )
    resamples <- map_dfr(bootstraps$splits, function(.data) {.data %>% analysis() %>% summary_fun()} )
    summary <- rbind(      resamples %>% summarize_all(function(data) {quantile(data, 0.5)}) %>% mutate(aggregation = "Median"),
                           resamples %>% summarize_all(function(data) {quantile(data, 0.025)}) %>% mutate(aggregation = "Lower Bound (95% CI)"),
                           resamples %>% summarize_all(function(data) {quantile(data, 0.975)}) %>% mutate(aggregation = "Upper Bound (95% CI)"))
    summary
  }
  
  dirs = list.dirs(root_path,recursive = FALSE)
  baseline_dir = Filter(function(.) {grepl("-baseline", .)}, dirs)
  experiment_dirs = Filter(function(.) {!grepl("-baseline", .)}, dirs)
  print(experiment_dirs)
  baseline = read.simulation.results(paste0(baseline_dir,"/ReportHIVByAgeAndGender"), "baseline", stratify_columns = c("Year","Gender","Age","HasIntervention.LEN","HasIntervention.Oral_PrEP"), 
                                               summarize_columns = c("Newly.Infected","Newly.Tested.Positive", "Newly.Tested.Negative", "Population", 
                                                                     "Infected", "On_ART", "Died", "Died_from_HIV",
                                                                     "Tested.Ever", "Diagnosed"), 
                                               min_age_inclusive=15, max_age_inclusive=100) %>% filter(Year < 2060)
  baseline = calculate.pop_scaling_factor(baseline, 
                                          pop_scale_param_inst$year, 
                                          reference_population = pop_scale_param_inst$population,
                                          age_max_inclusive = pop_scale_param_inst$age_max_inc,
                                          age_min_inclusive = pop_scale_param_inst$age_min_inc)
  
  # ABOVE THIS LINE is functions that we will call
  # BELOW is the "main" script
  
  #EMODAnalyzeR::bigpurple.add_slurm_to_path()
  bigpurple_opts = list(partition = "a100_short", time = "12:00:00")
  inf_averted = lapply(as.list(experiment_dirs), 
                               inf_averted_fun)
  #slurmR::Slurm_lapply(as.list(experiment_dirs), 
                 #                    inf_averted_fun, 
                                     #sbatch_opt=bigpurple_opts, 
                                     #njobs = length(experiment_dirs), 
                                     #export = c("dalys_by_sim", "infections_averted_over_time", "baseline","get_infections_and_py", "pop_scale_param_inst"))
  
  inf_averted %>% bind_rows %>% 
    mutate(experiment = stringr::str_split(experiment, "/") %>% map(~ .[[9]]) %>% unlist()) %>% 
    write.csv(file=paste0(root_path,"/infections_averted.csv"))
    #ggplot + geom_point(aes(x=py_on_treatment ,y=infections.averted)) + scale_x_continuous(expand = c(0, 0)) + 
    #scale_y_continuous(expand = c(0, 0), limits = c(0, 4.5e6)) + geom_text(aes(x=py_on_treatment ,y=infections.averted, label=experiment, hjust=0))
  
  max_price_fun <- function(path) {
    experiment = 
      paste0(path,"/ReportHIVByAgeAndGender") %>%
      read.simulation.results("Intervention", stratify_columns = c("Year","Gender","Age","HasIntervention.LEN","HasIntervention.Oral_PrEP"), 
                              summarize_columns = c("Newly.Infected","Newly.Tested.Positive", "Newly.Tested.Negative", "Population", 
                                                    "Infected", "On_ART", "Died", "Died_from_HIV",
                                                    "Tested.Ever", "Diagnosed"), 
                              min_age_inclusive=15, max_age_inclusive=100) %>% filter(Year < 2060)
    plt = emodplot.incidence(baseline, experiment, 2020,2059)
    ggplot2::ggsave(paste0(path,"/incidence.png"),plot=plt)
    calc_max_price(baseline, experiment, cost_art) %>% mutate(experiment = path)
  }
  
  
  max_prices = lapply(as.list(experiment_dirs), 
                                    max_price_fun)
                                  #slurmR::Slurm_  #sbatch_opt=bigpurple_opts, 
                                    #njobs = length(experiment_dirs), 
                                    #export = c("dalys_by_sim", "calc_max_price", "baseline","get_infections_and_py", "emodplot.incidence","pop_scale_param_inst","cost_art","life_expectancy"))
  
  max_prices %>% bind_rows %>% 
      mutate(experiment = stringr::str_split(experiment, "/") %>% map(~ .[[9]]) %>% unlist()) %>% 
      write.csv(file=paste0(root_path,"/max_prices.csv"))
}
#test
run_report( root_path = "/gpfs/data/bershteynlab/EMOD/kaftad01/202401_SA_LEN_realistic_coverage/uw_output", 
            cost_art=187, 
            life_expectancy = 66, 
            pop_scale_param_inst = pop_scale_params(year=2009,population=33868111, age_min_inc=15, age_max_inc= 64))
