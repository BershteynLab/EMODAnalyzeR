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
report.calibration.results <- function(experiment_path, ingest_path, pop_scaling_factor = 1, figure_path = "./") {
  ingest_data <- read.ingest.file(ingest_path)

  data <- read.simulation.results(
    experiment_path, "calibration",
    min_age_inclusive = -1,
    max_age_inclusive = 200,
    stratify_columns = c("Year","NodeId", "Gender", "Age"),
    summarize_columns = c("Population","Infected", "On_ART", "Newly.Infected"))

  data$pop_scaling_factor = ifelse(pop_scaling_factor < 1, 1/pop_scaling_factor, pop_scaling_factor)

  if (any("Obs-ARTCoverage" == names(ingest_data))) {
    plottopics.artcoverage <- ingest_data[['Obs-ARTCoverage']] %>%
      select(Province, AgeBin) %>%
      distinct() %>%
      agebin.to.min.max() %>%
      filter(
        (!is.na(Province)) &
          (!is.na(AgeBin)) )

    for (i_row in seq(1, nrow(plottopics.artcoverage))) {
      actuals <- ingest_data[['Obs-ARTCoverage']] %>%
        filter(Province == plottopics.artcoverage[[i_row,'Province']] &
                 AgeBin == plottopics.artcoverage[[i_row,'AgeBin']] )


      this.sim.data = data %>%
        filter(Age > plottopics.artcoverage$age.min[i_row] &
                 Age < ( plottopics.artcoverage$age.max[i_row] + 1 ) )

      node_id = ifelse(str_to_lower(plottopics.artcoverage[[i_row,'Province']]) == 'all', 'all', ingest_data[["site_map"]][[plottopics.artcoverage[[i_row,'Province']]]])

      emodplot.artcoverage(this.sim.data, 2000, 2025, node_id = node_id) +
        geom_point(data = actuals, aes(x=Year, y=Incidence)) +
        geom_errorbar(data = actuals, aes(x=Year, ymin=lb, ymax=ub), color="black", width=2, size=1)

      ggsave(paste0(figure_path, '/',
                    plottopics.artcoverage[[i_row,'Province']],
                    plottopics.artcoverage[[i_row,'age.min']],
                    '-',
                    plottopics.artcoverage[[i_row,'age.max']],
                    "ART_Coverage.png"))

    }
  }
  if (any("Obs-OnART" == names(ingest_data))) {
    plottopics.on_art <- ingest_data[['Obs-OnART']] %>%
      select(Province, AgeBin) %>%
      distinct() %>%
      agebin.to.min.max() %>%
      filter(
        (!is.na(Province)) &
          (!is.na(AgeBin)) )

    for (i_row in seq(1, nrow(plottopics.on_art))) {
      actuals <- ingest_data[['Obs-OnART']] %>%
        filter(Province == plottopics.on_art[[i_row,'Province']] &
                 AgeBin == plottopics.on_art[[i_row,'AgeBin']] )


      if ( str_to_lower(plottopics.on_art[[i_row,'Province']]) == 'all' ) {
        this.sim.data = data %>%
          filter(Age > plottopics.on_art$age.min[i_row] &
                   Age < ( plottopics.on_art$age.max[i_row] + 1 ) ) %>%
          dplyr::group_by(Year, Gender, sim.id, scenario_name) %>%
          dplyr::summarize(Population = sum(Population), On_ART = sum(On_ART), pop_scaling_factor = mean(pop_scaling_factor))
      } else {
        this.sim.data = data %>%
          filter(Age > plottopics.on_art$age.min[i_row] &
                   Age < ( plottopics.on_art$age.max[i_row] + 1 ) &
                   ingest_data[["site_map"]][[plottopics.on_art[[i_row,'Province']]]] == NodeId) %>%
          dplyr::group_by(Year, Gender, sim.id, scenario_name) %>%
          dplyr::summarize(Population = sum(Population), On_ART = sum(On_ART), pop_scaling_factor = mean(pop_scaling_factor))
      }

      emodplot.art(this.sim.data, 2000, 2025) +
        geom_point(data = actuals, aes(x=Year, y=OnART)) +
        geom_errorbar(data = actuals, aes(x=Year, ymin=lb, ymax=ub), color="black", width=2, size=1)

      ggsave(paste0(figure_path, '/',
                    plottopics.on_art[[i_row,'Province']],
                    plottopics.on_art[[i_row,'age.min']],
                    '-',
                    plottopics.on_art[[i_row,'age.max']],
                    "OnART.png"))

    }
  }

  if (any("Obs-Incidence" == names(ingest_data))) {
    plottopics.incidence <- ingest_data[['Obs-Incidence']] %>%
      select(Province, AgeBin) %>%
      distinct() %>%
      agebin.to.min.max() %>%
      filter(
        (!is.na(Province)) &
          (!is.na(AgeBin)) )

    for (i_row in seq(1, nrow(plottopics.incidence))) {
      actuals <- ingest_data[['Obs-Incidence']] %>%
                    filter(Province == plottopics.incidence[[i_row,'Province']] &
                             AgeBin == plottopics.incidence[[i_row,'AgeBin']] )


      if ( str_to_lower(plottopics.incidence[[i_row,'Province']]) == 'all' ) {
        this.sim.data = data %>%
          filter(Age > plottopics.incidence$age.min[i_row] &
                   Age < ( plottopics.incidence$age.max[i_row] + 1 ) ) %>%
          dplyr::group_by(Year, Gender, sim.id, scenario_name) %>%
          dplyr::summarize(Population = sum(Population), Infected = sum(Infected), Newly.Infected = sum(Newly.Infected))
      } else {
        this.sim.data = data %>%
          filter(Age > plottopics.incidence$age.min[i_row] &
                   Age < ( plottopics.incidence$age.max[i_row] + 1 ) &
                   ingest_data[["site_map"]][[plottopics.incidence[[i_row,'Province']]]] == NodeId) %>%
          dplyr::group_by(Year, Gender, sim.id, scenario_name) %>%
          dplyr::summarize(Population = sum(Population), Infected = sum(Infected), Newly.Infected = sum(Newly.Infected))
      }

      emodplot.incidence(this.sim.data, 2000, 2025) +
        geom_point(data = actuals, aes(x=Year, y=Incidence)) +
        geom_errorbar(data = actuals, aes(x=Year, ymin=lb, ymax=ub), color="black", width=2, size=1)

      ggsave(paste0(figure_path, '/',
                    plottopics.incidence[[i_row,'Province']],
                    plottopics.incidence[[i_row,'age.min']],
                    '-',
                    plottopics.incidence[[i_row,'age.max']],
                    "Incidence.png"))

    }
  }

  if (any("Obs-Prevalence" == names(ingest_data))) {

    plottopics.prevalence <- ingest_data[['Obs-Prevalence']] %>%
                              select(Province, AgeBin) %>%
                              distinct() %>%
                              agebin.to.min.max() %>%
                              filter(
                                (!is.na(Province)) &
                                  (!is.na(AgeBin)) )

    for (i_row in seq(1, nrow(plottopics.prevalence))) {
      actuals <- ingest_data[['Obs-Prevalence']] %>%
        filter(Province == plottopics.prevalence[[i_row,'Province']] &
                 AgeBin == plottopics.prevalence[[i_row,'AgeBin']] )


      if ( str_to_lower(plottopics.prevalence[[i_row,'Province']]) == 'all' ) {
        this.sim.data = data %>%
          filter(Age > plottopics.prevalence$age.min[i_row] &
                   Age < ( plottopics.prevalence$age.max[i_row] + 1 ) ) %>%
          dplyr::group_by(Year, Gender, sim.id, scenario_name) %>%
          dplyr::summarize(Population = sum(Population), Infected = sum(Infected))
      } else {
        this.sim.data = data %>%
          filter(Age > plottopics.prevalence$age.min[i_row] &
                   Age < ( plottopics.prevalence$age.max[i_row] + 1 ) &
                   ingest_data[["site_map"]][[plottopics.prevalence[[i_row,'Province']]]] == NodeId) %>%
          dplyr::group_by(Year, Gender, sim.id, scenario_name) %>%
          dplyr::summarize(Population = sum(Population), Infected = sum(Infected))
      }

      emodplot.prevalence(this.sim.data, 2000, 2025) +
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
