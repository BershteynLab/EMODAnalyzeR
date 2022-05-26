library(EMODAnalyzeR)
path.intervention <- "C:/Users/kaftad01/Documents/EMOD/2022-04_SouthAfricaDPP/SouthAfricaDPPNew/SouthAfricaDPP/Baseline-campaign_RSA_baseline_DPP_04192022-Baseline/ReportHIVByAgeAndGender"
path.counterfactual <- "C:/Users/kaftad01/Documents/EMOD/2022-04_SouthAfricaDPP/SouthAfricaDPPNew/SouthAfricaDPP/Baseline-campaign_RSA_Counterfactual_no_DPP_04192022-Counterfactual_1/ReportHIVByAgeAndGender"

data.intervention <-read.simulation.results(path.intervention,
                                            'DPP',
                                            summarize_columns = c("Newly.Infected",
                                                                  "Newly.Tested.Positive",
                                                                  "Newly.Tested.Negative","Population",
                                                                  "Infected", "On_ART","Died", "Died_from_HIV",
                                                                  "Tested.Past.Year.or.On_ART", "Tested.Ever",
                                                                  "Diagnosed","Received_DPP"),
                                            stratify_columns = c("Year","Age"),
                                            min_age_inclusive = 0,
                                            max_age_inclusive = 200)

data.nointervention <-read.simulation.results(path.counterfactual,
                                              'NoDPP',
                                              summarize_columns = c("Newly.Infected",
                                                                    "Newly.Tested.Positive",
                                                                    "Newly.Tested.Negative","Population",
                                                                    "Infected", "On_ART","Died", "Died_from_HIV",
                                                                    "Tested.Past.Year.or.On_ART", "Tested.Ever",
                                                                    "Diagnosed", "Received_DPP"),
                                              stratify_columns = c("Year","Age"),
                                              min_age_inclusive = 0,
                                              max_age_inclusive = 200)

data.intervention <- calculate.pop_scaling_factor(data.intervention, 2019, 58780000, age_min_inclusive = 0, age_max_inclusive = 110)
data.nointervention <- calculate.pop_scaling_factor(data.nointervention, 2019, 58780000, age_min_inclusive = 0, age_max_inclusive = 110)
baseline <- run.comparison(data.intervention, data.nointervention, 0.03, "Baseline", 2055 )

