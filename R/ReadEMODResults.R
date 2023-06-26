# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# library(openxlsx)
# library(readxl)
# library(dplyr)
# library(ggplot2)
# library(reshape2)
# library(plyr)
# library(plotrix)
# library(data.table)
# library(tidyr)
# library(stringr)

read.each_sim_by_age_and_gender <- function (filename, summarize_columns, stratify_columns, min_age_inclusive = 0, max_age_inclusive = Inf) {
  data.table::fread(filename, check.names = TRUE) %>%
    filter( (Age >= min_age_inclusive) & (Age <= max_age_inclusive) ) %>%
    group_by_at(stratify_columns) %>%
    summarise_at(summarize_columns, sum, na.rm=T) %>%
    ungroup()
}


#' Read results of an EMOD simulation
#' @details The results of an EMOD simulation are stored in a series of csv files titled "ReportHIVByAgeAndGender.csv". One of these files exists for each
#' simulation run (typically 250 files). This function reads and aggregates those files into a single tibble
#' @param results_path string pointing to the folder which contains the ReportHIVByAgeAndGender.csv files
#' @param scenario_name Provide a string label for the scenario being read in. For example, you might use "baseline" for the baseline scenario.
#' @param summarize_columns a vector of strings containing names of columns to be aggregated via summation. Note that spaces in column names are replaced by a period ("."). For example, "Newly Infected" becomes "Newly.Infected".
#' @param stratify_columns a vector of strings containing names of columns by which we will stratify the data. For example, we might want to have a separate row in the dataset for each year, so we would set stratify_columns = c("Year")
#' @param min_age_inclusive an integer representing the minimum age to keep while reading the data (all ages below will be filtered out)
#' @param max_age_inclusive an integer representing the maximum age to keep while reading the data (all ages above will be filtered out)
#' @return A tibble with columns incidence and Year
read.simulation.results <- function(results_path,
                                  scenario_name,
                                  summarize_columns = c("Newly.Infected", "Newly.Tested.Positive",
                                                        "Newly.Tested.Negative","Population",
                                                        "Infected", "On_ART","Died", "Died_from_HIV",
                                                        "Tested.Past.Year.or.On_ART", "Tested.Ever",
                                                        "Diagnosed"),
                                  stratify_columns = c("Year", "Gender"),
                                  min_age_inclusive = 15,
                                  max_age_inclusive = 49) {
  ### Read 250 simulation and aggregate by age 15+
  file_list = list.files(results_path, full.names = F, recursive=F)

  n_runs_to_analyze = length(file_list)
  print(paste('Found',as.character(n_runs_to_analyze),'output files for scenario',scenario_name))
  data.list <- list()
  for (i in seq(1,length(file_list),1)){
    f <- paste(results_path, file_list[i], sep="/")
    data.list[[i]] <- read.each_sim_by_age_and_gender(f, summarize_columns, stratify_columns, min_age_inclusive, max_age_inclusive )
    data.list[[i]]$sim.id <- paste0(f)
    data.list[[i]]$scenario_name <- scenario_name
    print(paste0("Done Reading File ", i))
  }

  bind_rows(data.list)
}

#' Read results of an EMOD simulation from its original location off the BigPurple filesystem
#' @details When a simulation is run on BigPurple, dtk-tools creates a simulation folder somewhere on BigPurple (the folder which contains the simulation folder
#' is specified in simtools.ini using the parameter "sim_root"). In this folder is a set of folders - each one representing a different run of the
#' simulation. These folders will look something like "Simulation_6CGUFHY7". The results of each simulation are stored within these folders.
#' The results of an EMOD simulation are stored in a series of csv files titled "ReportHIVByAgeAndGender.csv". One of these files exists for each
#' simulation run (typically 250 files). This function reads and aggregates those files into a single tibble.
#' @param experiment_path string pointing to the folder which contains the Simulation_XXXXXXXX folders. For example, /gpfs/scratch/kaftad01/experiments/Baseline-campaign_Nyanza_baseline_03112021_NoPrEP-Baseline___2022_02_17_21_34_51_660565
#' @param scenario_name string for the name of the scenario being read. For example, you might use "baseline" for the baseline scenario.
#' @param summarize_columns a vector of strings containing names of columns to be aggregated via summation. Note that spaces in column names are replaced by a period ("."). For example, "Newly Infected" becomes "Newly.Infected".
#' @param stratify_columns a vector of strings containing names of columns by which we will stratify the data. For example, we might want to have a separate row in the dataset for each year, so we would set stratify_columns = c("Year")
#' @param min_age_inclusive an integer representing the minimum age to keep while reading the data (all ages below will be filtered out)
#' @param max_age_inclusive an integer representing the maximum age to keep while reading the data (all ages above will be filtered out)
#' @return A tibble with columns incidence and Year
read.simulation.results.bigpurple <- function(experiment_path,
                                    scenario_name,
                                    summarize_columns = c("Newly.Infected", "Newly.Tested.Positive",
                                                          "Newly.Tested.Negative","Population",
                                                          "Infected", "On_ART","Died", "Died_from_HIV",
                                                          "Tested.Past.Year.or.On_ART", "Tested.Ever",
                                                          "Diagnosed"),
                                    stratify_columns = c("Year", "Gender"),
                                    min_age_inclusive = 15,
                                    max_age_inclusive = 49,
                                    verbose = FALSE) {
  ### Read 250 simulation and aggregate by age 15+
  folder.list = Sys.glob(paste0(experiment_path, "/Simulation_*"))

  n_runs_to_analyze = length(folder.list)
  print(paste('Found',as.character(n_runs_to_analyze),'output files for scenario',scenario_name))
  data.list = list()
  for (i in seq(1,length(folder.list),1)){
    f <- paste(folder.list[i], "output/ReportHIVByAgeAndGender.csv", sep="/")
    raw.data <- fread(f, check.names = TRUE)
    data.list[[i]] <- read.each_sim_by_age_and_gender(f, summarize_columns, stratify_columns, min_age_inclusive, max_age_inclusive )
    data.list[[i]]$sim.id <- paste0(f)
    data.list[[i]]$scenario_name <- scenario_name
    if (verbose) print(paste0("Done Reading File ", i))
  }

  bind_rows(data.list)
}


read.ingest.sheet <- function(ingest_filename, sheet) {
  raw_data <- read_excel(ingest_filename, sheet)
  first_row <- last(which(raw_data[,1] == 'Year'))
  instrument_name <- names(raw_data)[2]
  sheet_data <- read_excel(ingest_filename, sheet, skip = first_row, .name_repair =  ~ make.names(make.unique(.)))
  if (any(names(sheet_data) == "two_sigma")) {
    bounds <- calculate.bounds.two_sigma(sheet_data[,instrument_name], sheet_data$two_sigma)
  } else {
    valid_rows <- (!is.na(sheet_data$effective_count)) & (!is.na(sheet_data[,instrument_name]))
    bounds <- data.frame(lb=vector(mode='numeric', length=length(valid_rows))) + NA
    bounds$ub <- NA
    bounds[valid_rows,] <- calculate.bounds.effective_count(
                            as.numeric(sheet_data[valid_rows,instrument_name] %>% unlist(use.names = FALSE)),
                            as.numeric(sheet_data[valid_rows, "effective_count"] %>% unlist(use.names = FALSE)))
  }

  sheet_data[,'lb'] = bounds$lb
  sheet_data[,'ub'] = bounds$ub
  sheet_data
}

read.ingest.file <- function(ingest_filename) {
  sheets <- Filter(function(x) {grepl('Obs-', x)}, excel_sheets(ingest_filename))
  datasets <- Map(function(x) {read.ingest.sheet(ingest_filename, x)}, sheets)
  site_data <- read_excel(ingest_filename, "Site")
  site_t = t(site_data)
  colnames(site_t) <- site_t[1,]
  site_t = site_t[-1,]
  sites = sapply(site_t[,'Node number'], as.numeric)
  names(sites) = site_t[,"Node name"]
  datasets[['site_map']] = sites
  datasets
}

