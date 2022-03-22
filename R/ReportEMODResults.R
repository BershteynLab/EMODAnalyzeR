agebin.to.min.max <- function(agebin) {
  agebin %>%
    mutate(
      age.min = as.numeric(
                            lapply(str_extract_all(AgeBin,"[0-9]+"), function(x) {x[1]} )),
      age.max = as.numeric(
                            lapply(str_extract_all(AgeBin,"[0-9]+"), function(x) {x[2]} )))

}

#' Report calibration fit of EMOD simulation
#'
#' @param experiment_path path on big purple where experiment was run. Typically this will look like
#' /gpfs/scratch/kaftad01/experiments/Zambia--0.02--rep7--testCalibrateSeedYear_iter32___2022_03_21_12_39_07_01727
report.calibration.results <- function(experiment_path, ingest_path) {
  ingest_data <- read.ingest.file(ingest_path)
  plottopics.prevalence <- ingest_data[['Obs-Prevalence']] %>%
                            select(Province, AgeBin) %>%
                            distinct() %>%
                            agebin.to.min.max()
  for (i_row in seq(1, nrow(plottopics.prevalence) - 1)){
    actuals <- ingest_data[['Obs-Prevalence']] %>%
                  filter(Province == plottopics.prevalence[[i_row,'Province']] &
                           AgeBin == plottopics.prevalence[[i_row,'AgeBin']] )
    data <- read.simulation.results.bigpurple(
                                      experiment_path, "calibration",
                                      min_age_inclusive = plottopics.prevalence$age.min[i_row],
                                      max_age_inclusive = plottopics.prevalence$age.max[i_row] + 1,
                                      stratify_columns = c("Year","NodeId", "Gender"),
                                      summarize_columns = c("Population","Infected")) %>%
            filter(ingest_data[["site_map"]][[plottopics.prevalence[[i_row,'Province']]]] == NodeId)

    plot.prevalence(data, 2000, 2025) +
      geom_point(aes(x=Year, y=Prevalence)) +
      geom_errorbar(data = actuals, aes(x=Year, ymin=lb, ymax=ub), color="black", width=2, size=1)

    ggsave(paste0(plottopics.prevalence[[i_row,'Province']], plottopics.prevalence[[i_row,'age.min']], '-', plottopics.prevalence[[i_row,'age.max']], ".png"))

  }
  plt
}

#d = report.calibration.results("C:/Users/kaftad01/Documents/EMOD/ReportHIVByAgeAndGender (1) - Copy", "D:/Dropbox (NYU CEDS)/EMOD HIV User Group Shared Folder/input_file_bundles/Nyanza_calibration/k20220218_Nyanza_baseline_from_PrEP/Data/calibration_ingest_form_Nyanza.xlsm")
