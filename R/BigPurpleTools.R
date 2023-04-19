
bigpurple.add_slurm_to_path <- function() {
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(old_path, "/cm/shared/apps/slurm/current/bin/", sep = ":"))
  T
}

bigpurple.run_on_each_sim <- function(path,
                                      func,
                                      summarize_columns = c("Newly.Infected", "Newly.Tested.Positive",
                                                             "Newly.Tested.Negative","Population",
                                                             "Infected", "On_ART","Died", "Died_from_HIV",
                                                             "Tested.Past.Year.or.On_ART", "Tested.Ever",
                                                             "Diagnosed"),
                                       stratify_columns = c("Year", "Gender"),
                                       min_age_inclusive = 15,
                                       max_age_inclusive = 49,
                                       bigpurple_opts=list(partition="a100_short", time="12:00:00")){
  if (!slurm_available()) {
    bigpurple.add_slurm_to_path()
  }
  read_fun = partial(read.each_sim_by_age_and_gender,
                      summarize_columns=summarize_columns,
                      stratify_columns=stratify_columns,
                      min_age_inclusive=min_age_inclusive,
                      max_age_inclusive=max_age_inclusive)

  apply_fun = compose(func, read_fun)
  files = as.list(list.files(path, recursive = F, full.names = T))
  Slurm_lapply(files, apply_fun, sbatch_opt=bigpurple_opts)
}
