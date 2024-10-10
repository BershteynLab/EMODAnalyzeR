filter.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    filter(...)
}

filter.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    filter(...)
}

c_EMODSimList <- function(l_) {
  i = 1
  outlist = list()
  for (item in l_) {
    for (item_ in item) {
      outlist[[i]] = item_
      i = i + 1
    }
  }
  class(outlist) <- "EMODSimList"
  outlist
}

c.EMODSimList <- function(...) {
  l_items = list(...)
  suppressWarnings(c_EMODSimList(l_items))
}


as_tibble.EMODSimList <- function(sims){
  sims %>%
    lapply(function(sim) {sim@load_fun()}) %>%
    bind_rows()
}

as_tibble.EMODSim <- function(sim){
  sim@load_fun()
}

as.data.frame.EMODSim <- function(sim) {
  sim %>%
    as_tibble %>%
    as.data.frame
}

as.data.frame.EMODSimList <- function(sims) {
  sims %>%
    as_tibble %>%
    as.data.frame
}

fortify.EMODSim <- function(sim) {
  sim %>%
    as_tibble %>%
    fortify
}

fortify.EMODSimList <- function(sims) {
  sims %>%
    as_tibble %>%
    fortify
}

`[.EMODSimList` <- function(.data,i,j) {
  x = (.data %>% as_tibble)
  if (missing(i)) {
    return(x[j])
  } else if(missing(j)) {
    return(x[i])
  } else {
    return(x[i,j])
  }
}

`[<-.EMODSimList` <- function(.data, i, j, value) {
  x = (.data %>% as_tibble)
  if (missing(i)) {
    x[j] = value
  } else if(missing(j)) {
    x[i] = value
  } else {
    x[i,j] = value
  }
  x
}

`[.EMODSim` <- function(.data,i,j) {
  x = (.data %>% as_tibble)
  if (missing(i)) {
    return(x[j])
  } else if(missing(j)) {
    return(x[i])
  } else {
    return(x[i,j])
  }
}

`[<-.EMODSim` <- function(.data, i, j, value) {
  x = (.data %>% as_tibble)
  if (missing(i)) {
    x[j] = value
  } else if(missing(j)) {
    x[i] = value
  } else {
    x[i,j] = value
  }
  x
}

`$.EMODSim` <- function(.data, column) {
  (.data %>% as_tibble)[,column]
} 

`$<-.EMODSim` <- function(.data, name, value) {
  x = (.data %>% as_tibble)
  x[,name] = value
  x
}

`$.EMODSimList` <- function(.data, column) {
  (.data %>% as_tibble)[,column]
} 

`$<-.EMODSimList` <- function(.data, name, value) {
  x = (.data %>% as_tibble)
  x[,name] = value
  x
}

arrange.EMODSim <- function (.data, ...) {
  .data %>%
    as_tibble %>%
    arrange(...)
}

arrange.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    arrange(...)
}

distinct.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    distinct(...)
}
filter.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    filter(...)
}
slice.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice(...)
}
slice_head.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice_head(...)
}
slice_tail.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice_tail(...)
}
slice_min.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice_min(...)
}
slice_max.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice_max(...)
}
slice_sample.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    slice_sample(...)
}

glimpse.EMODSim <- function(.data) {
  .data %>%
    as_tibble %>%
    glimpse()
}

mutate.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    mutate(...)
}
pull.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    pull(...)
}
relocate.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    relocate(...)
}
rename.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    rename(...)
}
rename_with.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    rename_with(...)
}
select.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    select(...)
}
count.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    count(...)
}
tally.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    tally(...)
}
add_count.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    add_count(...)
}
add_tally.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    add_tally(...)
}
group_by.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    group_by(...)
}
ungroup.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    ungroup(...)
}
rowwise.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    rowwise(...)
}
summarise.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    summarise(...)
}
reframe.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    reframe(...)
}
n.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    n(...)
}
cur_group.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    cur_group(...)
}
cur_group_id.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    cur_group_id(...)
}
cur_group_rows.EMODSim <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    cur_group_rows(...)
}
cur_column.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    cur_column(...)
}
pick.EMODSim <- function(.data, ...){
  .data %>%
    as_tibble %>%
    pick(...)
}







distinct.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    distinct(...)
}
filter.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    filter(...)
}
slice.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice(...)
}
slice_head.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice_head(...)
}
slice_tail.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice_tail(...)
}
slice_min.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice_min(...)
}
slice_max.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    slice_max(...)
}
slice_sample.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    slice_sample(...)
}

glimpse.EMODSimList <- function(.data) {
  .data[[1]] %>%
    as_tibble %>%
    glimpse()
}  

mutate.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    mutate(...)
}
pull.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    pull(...)
}
relocate.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    relocate(...)
}
rename.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    rename(...)
}
rename_with.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    rename_with(...)
}
select.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    select(...)
}
count.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    count(...)
}
tally.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    tally(...)
}
add_count.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    add_count(...)
}
add_tally.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    add_tally(...)
}
group_by.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    group_by(...)
}
ungroup.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    ungroup(...)
}
rowwise.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    rowwise(...)
}
summarise.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    summarise(...)
}
reframe.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    reframe(...)
}
n.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    n(...)
}
cur_group.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    cur_group(...)
}
cur_group_id.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    cur_group_id(...)
}
cur_group_rows.EMODSimList <- function(.data, ...) {
  .data %>%
    as_tibble %>%
    cur_group_rows(...)
}
cur_column.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    cur_column(...)
}
pick.EMODSimList <- function(.data, ...){
  .data %>%
    as_tibble %>%
    pick(...)
}



