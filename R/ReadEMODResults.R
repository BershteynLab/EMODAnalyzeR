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
library(easypackages)
libraries("openxlsx", "readxl", "questionr", "dplyr", "ggplot2", "reshape2", "plyr", "ggpirate", "plotrix", "maditr", "rmarkdown")

ReadSimulationResults <- function(results_path, scenario_names) {
  ### Read 250 simulation and aggregate by age 15+
  for(curr_scenario_name in scenario_names){
    file_list = list.files(results_path, full.names = F, recursive=F)

    n_runs_to_analyze = length(file_list)
    print(paste('Found',as.character(n_runs_to_analyze),'output files for scenario',curr_scenario_name))

    for (i in seq(1,length(file_list),1)){
      f <- paste(results_path, file_list[i], sep="/")

      dat <- read.csv(f)
      dat$sim.id <- paste0(file_list[i])
      if (i==1) {
        dat.all.files <- dat
      } else {
        dat.all.files <- rbind(dat.all.files, dat)
        print(paste('Analyzed file for all year infections:',i))
      }
    }

    #This allows to load in the simulation results from multiple scenarios/campaign files
    if (curr_scenario_name==scenario_names[1]) {
      dat.all.scenarios <- dat.all.files
    }
    else {
      dat.all.scenarios <- rbind(dat.all.scenarios, scenario_allyr.all)
      print(paste('Analyzed the scenario for all year infections:',curr_scenario_name))
    }
  }
  dat.all.scenarios
}

SummarizeEachSimByAgeAndGender(data, min_age_inclusive = 0, max_age_inclusive = Inf) {
  data %>%
  filter( (Age >= min_age_inclusive) & (Age <= min_age_inclusive) ) %>%
  group_by(Year, Gender, sim.id) %>%
  summarise_at(c("Newly.Infected", "Newly.Tested.Positive", "Newly.Tested.Negative",
                  "Population", "Infected", "On_ART",
                  "Died", "Died_from_HIV", "Tested.Past.Year.or.On_ART", "Tested.Ever", "Diagnosed"), sum, na.rm=T)
}
