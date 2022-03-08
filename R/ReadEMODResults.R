# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(openxlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)
library(plotrix)
library(data.table)

SummarizeEachSimByAgeAndGender <- function (data, summarize_columns, min_age_inclusive = 0, max_age_inclusive = Inf) {
  data %>%
    filter( (Age >= min_age_inclusive) & (Age <= max_age_inclusive) ) %>%
    group_by(Year, Gender) %>%
    summarise_at(summarize_columns, sum, na.rm=T)
}

read.simulation.results <- function(results_path,
                                  scenario_name,
                                  summarize_columns = c("Newly.Infected", "Newly.Tested.Positive",
                                                        "Newly.Tested.Negative","Population",
                                                        "Infected", "On_ART","Died", "Died_from_HIV",
                                                        "Tested.Past.Year.or.On_ART", "Tested.Ever",
                                                        "Diagnosed"),
                                  min_age_inclusive = 15,
                                  max_age_inclusive = 49) {
  ### Read 250 simulation and aggregate by age 15+
  file_list = list.files(results_path, full.names = F, recursive=F)

  n_runs_to_analyze = length(file_list)
  print(paste('Found',as.character(n_runs_to_analyze),'output files for scenario',scenario_name))

  for (i in seq(1,length(file_list),1)){
    f <- paste(results_path, file_list[i], sep="/")
    raw.data <- fread(f, check.names = TRUE)
    dat <- SummarizeEachSimByAgeAndGender(raw.data, summarize_columns, min_age_inclusive, max_age_inclusive )
    dat$sim.id <- paste0(file_list[i])
    dat$scenario_name <- scenario_name
    if (i==1) {
      dat.all.files <- dat
    } else {
      dat.all.files <- rbind(dat.all.files, dat)
      print(paste('Analyzed file for all year infections:',i))
    }
  }

  dat.all.files
}


