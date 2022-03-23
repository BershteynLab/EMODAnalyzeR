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
report.calibration.results <- function(experiment_path, ingest_path, figure_path = "./") {
  ingest_data <- read.ingest.file(ingest_path)
  plottopics.prevalence <- ingest_data[['Obs-Prevalence']] %>%
                            select(Province, AgeBin) %>%
                            distinct() %>%
                            agebin.to.min.max() %>%
                            filter(
                              (!is.na(Province)) &
                                (!is.na(AgeBin)) )

  data <- read.simulation.results.bigpurple(
    experiment_path, "calibration",
    min_age_inclusive = -1,
    max_age_inclusive = 200,
    stratify_columns = c("Year","NodeId", "Gender", "Age"),
    summarize_columns = c("Population","Infected"))

  if (any("Obs-Prevalence" == names(ingest_data))) {

    for (i_row in seq(1, nrow(plottopics.prevalence))) {
      actuals <- ingest_data[['Obs-Prevalence']] %>%
                    filter(Province == plottopics.prevalence[[i_row,'Province']] &
                             AgeBin == plottopics.prevalence[[i_row,'AgeBin']] )


      if ( str_to_lower(plottopics.prevalence[[i_row,'Province']]) == 'all' ) {
        this.sim.data = data %>%
          filter(Age > plottopics.prevalence$age.min[i_row] &
                   Age < ( plottopics.prevalence$age.max[i_row] + 1 ) ) %>%
          dplyr::group_by(Year, Gender, sim.id) %>%
          dplyr::summarize(Population = sum(Population), Infected = sum(Infected))
      } else {
        this.sim.data = data %>%
          filter(Age > plottopics.prevalence$age.min[i_row] &
                   Age < ( plottopics.prevalence$age.max[i_row] + 1 ) &
                   ingest_data[["site_map"]][[plottopics.prevalence[[i_row,'Province']]]] == NodeId) %>%
          dplyr::group_by(Year, Gender, sim.id, scenario_name) %>%
          dplyr::summarize(Population = sum(Population), Infected = sum(Infected))
      }

      plot.prevalence(this.sim.data, 2000, 2025) +
        geom_point(data = actuals, aes(x=Year, y=Prevalence)) +
        geom_errorbar(data = actuals, aes(x=Year, ymin=lb, ymax=ub), color="black", width=2, size=1)

      ggsave(paste0(figure_path, '/',
                    plottopics.prevalence[[i_row,'Province']],
                    plottopics.prevalence[[i_row,'age.min']],
                    '-',
                    plottopics.prevalence[[i_row,'age.max']],
                    "Prevalence.png"))

    }
  }
}

#d = report.calibration.results("C:/Users/kaftad01/Documents/EMOD/ReportHIVByAgeAndGender (1) - Copy", "D:/Dropbox (NYU CEDS)/EMOD HIV User Group Shared Folder/input_file_bundles/Nyanza_calibration/k20220218_Nyanza_baseline_from_PrEP/Data/calibration_ingest_form_Nyanza.xlsm")
