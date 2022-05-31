#To use the EMODAnalyzeR Package, follow the instructions below
# Make sure you are running R 4.0 or greater
# Make sure you have devtools installed (if not, run install.packages(“devtools”) )
# Run library(devtools)
# Run install_github(“BershteynLab/EMODAnalyzeR”)

library(EMODAnalyzeR)

# Paths where you load data from
path.intervention <- "C:/Users/kaftad01/Documents/EMOD/2022-04_SouthAfricaDPP/SouthAfricaDPPNew/SouthAfricaDPP/Baseline-campaign_RSA_baseline_DPP_04192022-Baseline/ReportHIVByAgeAndGender"
path.counterfactual <- "C:/Users/kaftad01/Documents/EMOD/2022-04_SouthAfricaDPP/SouthAfricaDPPNew/SouthAfricaDPP/Baseline-campaign_RSA_Counterfactual_no_DPP_04192022-Counterfactual_1/ReportHIVByAgeAndGender"

treatment_initiated_col_name <- "Received_DPP" # Replace with column name for intervention initiated counter i.e., Received_PrEP
on_treatment_col_name <- "Received_DPP" #"On_DPP" # Replace with column name for number of people on intervention. i.e., On_PrEP


data.intervention <-read.simulation.results(path.intervention,
                                            'DPP',
                                            summarize_columns =  c("Newly.Infected",
                                                                   "Population",
                                                                   "Infected",
                                                                   "On_ART",
                                                                   "Died_from_HIV",
                                                                   treatment_initiated_col_name,
                                                                   on_treatment_col_name),
                                            stratify_columns = c("Year","Age"),
                                            min_age_inclusive = 0,
                                            max_age_inclusive = 200)

data.nointervention <-read.simulation.results(path.counterfactual,
                                              'NoDPP',
                                              summarize_columns = c("Newly.Infected",
                                                                    "Population",
                                                                    "Infected",
                                                                    "On_ART",
                                                                    "Died_from_HIV",
                                                                    "Received_DPP",
                                                                    treatment_initiated_col_name,
                                                                    on_treatment_col_name),
                                              stratify_columns = c("Year","Age"),
                                              min_age_inclusive = 0,
                                              max_age_inclusive = 200)

data.intervention <- calculate.pop_scaling_factor(data.intervention, 2019, 58780000, age_min_inclusive = 0, age_max_inclusive = 110)
data.nointervention <- calculate.pop_scaling_factor(data.nointervention, 2019, 58780000, age_min_inclusive = 0, age_max_inclusive = 110)
baseline <- icer.run.comparison(data.intervention,
                           data.nointervention,
                           0.03,
                           "Baseline",
                           2055,
                           name.treatment_population_column = on_treatment_col_name,
                           name.treatment_initiation_column = treatment_initiated_col_name )

