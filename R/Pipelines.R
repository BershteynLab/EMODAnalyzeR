emodrun_by_sim.local <- function(proc_function, eMODSims) {
  eMODSims %>%
    as_tibble %>%
    proc_function
}


bigpurple.add_slurm_to_path <- function() {
  old_path <- Sys.getenv("PATH")
  big_purple_slurm_path <- "/cm/shared/apps/slurm/current/bin/"
  if (!grepl(big_purple_slurm_path, old_path)) {
    Sys.setenv(PATH = paste(old_path, "/cm/shared/apps/slurm/current/bin/", sep = ":"))  
  }
  
  T
}


emodrun_by_sim.slurm <- function(proc_function, 
                                 eMODSims, 
                                 bigpurple_opts=list(
                                   partition="a100_short", 
                                   time="12:00:00")) {
  if (class(eMODSims) != "EMODSimList") {
    stop("ERROR: emodrun_by_sim.slurm expecting type EMODSimList. \n EMODSimList is returned by read.simulation.results, and will change classes if any dplyr functions are run on it.")
  }
  class(eMODSims)<-"list"
  class(proc_function)<-"function"
  if (!slurm_available()) {
    bigpurple.add_slurm_to_path()
  }
  l_results = Slurm_lapply(eMODSims, proc_function, sbatch_opt=bigpurple_opts, njobs=length(eMODSims), job_name=random_job_name())
  if (any(class(l_results[[1]]) == "data.frame")) {
    return(bind_rows(l_results))
  } else {
    return (l_results)
  }
}


