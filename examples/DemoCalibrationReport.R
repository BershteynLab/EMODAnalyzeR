library(devtools)
install_github("BershteynLab/EMODAnalyzeR")

library(EMODAnalyzeR)

# insert path to your reportbyageandgender files
path_to_report_by_age_and_gender <- "C:/Users/kaftad01/Documents/EMOD/demo_csv/ReportHIVByAgeAndGender/"

# insert path to ingest file. For Nyanza, you can download from https://www.dropbox.com/s/wkylqakjxkyzffb/calibration_ingest_form_Nyanza.xlsm?dl=0
# ingest file contains data used for calibration (prevalence, etc)
path_to_ingest_file <- "C:/Users/kaftad01/Documents/EMOD/2020-22Nyanza/calibration_ingest_form_Nyanza.xlsm"

# insert path to where you want the figure files to appear
figure_output_folder <- "C:/Users/kaftad01/Documents/"

# population scaling factor
pop_scaling_factor = 0.002

report.calibration.results(path_to_report_by_age_and_gender, path_to_ingest_file, figure_path = figure_output_folder, pop_scaling_factor = pop_scaling_factor)
