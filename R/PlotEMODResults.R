library(ggplot2)

colors = c("#0072b2","#009e73","#cc79a7")

#' Plot by gender
#' @rdname plot.by_gender
#' @details plots a metric in data. First, it faintly plots all simulations. Then it plots a bold line of the mean of all simulations
#' @param data A tibble returned from read.simulation.results(..., stratify_columns = c("Year", "Gender"), aggregate_columns = c("Population","Newly.Infected", "Infected"))
#' More aggregate columns can be used, but more stratify columns will cause problems in the plot.
#' @param date.start integer year to start the plotting (i.e., 2000)
#' @param date.end integer year to end the plotting (i.e., 2030)
#' @param col2plot string of column name you want to plot (i.e., "Infected")
#' @param title string of plot title (i.e., "Infected Plot")
#' @param unit string of the units for col2plot (i.e., "(Number of People)")
#' @return a ggplot with all data plotted
emodplot.by_gender <- function(data,
                         date.start,
                         date.end,
                         col2plot,
                         title="",
                         unit="") {
  nScenarios = length(unique(data$scenario_name))
  if (nScenarios == 1) {
    colors2plot <- c('blue3')
  } else {
    colors2plot <- colors[1:nScenarios]
  }
  data <- data %>% mutate(Gender = case_when(Gender==0 ~ "Male", Gender==1 ~ "Female"))
  data['col2plot'] = data[col2plot]
  data.mean <- data %>%
    group_by(Year, Gender, scenario_name) %>%
    dplyr::summarise(mean.col2plot = mean(col2plot), .groups = 'keep')
  ggplot(data=subset(data, (date.start <= Year) & (Year <= date.end))) +
    geom_point(size=1.0, aes(x=Year, y=col2plot*1, group=sim.id, color=scenario_name), alpha=0.005) +
    geom_line(data=subset(data.mean, (date.start <= Year) & (Year <= date.end)), 
              aes(x=Year, y=mean.col2plot*1, group=scenario_name, color=scenario_name), size=1.5) +
    # geom_point(data = prevalence.data, size=2, color = "black", aes(x=Year, y=Prevalence*1)) +
    # geom_errorbar(data = prevalence.data, aes(x=Year, ymin=lb*1, ymax=ub*1), color="black", width=2, size=1) +
    facet_wrap(~ Gender, ncol=2) +
    xlab("Year")+
    xlim(c(date.start, date.end)) +
    ylab(paste0(col2plot, " ", unit))+
    theme_bw(base_size=16) +
    guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
    scale_x_continuous(breaks = seq(date.start,date.end,10)) +
    theme(legend.position="bottom") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(strip.background = element_rect(colour="black", fill="white")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_color_manual(values=colors2plot) +
    labs(title= title)
}


#' Plot prevalence of hiv stratified by gender
#' @description plots prevalence of EMOD simulations. First, it faintly plots all simulations. Then it plots a bold line of the mean of all simulations
#' @param data A tibble returned from read.simulation.results(..., stratify_columns = c("Year", "Gender"), aggregate_columns = c("Population","Newly.Infected", "Infected")).
#' More aggregate columns can be used, but more stratify columns will cause problems in the plot.
#' @param date.start integer year to start the plotting (i.e., 2000)
#' @param date.end integer year to end the plotting (i.e., 2030)
#' @return a ggplot with prevalence plotted

emodplot.prevalence <- function(data,
                           date.start,
                           date.end,
                           title = "HIV prevalence") {
  data <- data %>% 
    group_by_at(Year, Gender, sim.id, scenario_name) %>% 
    summarize(Infected = sum(Infected), Population = sum(Population), .groups = 'keep') %>% 
    mutate(Prevalence = case_when(Population == 0 ~ 0,
                                  Population > 0 ~ Infected / Population))
  y.lim.max <- min(max(data$Prevalence) * 1.2, 1)
  p <- emodplot.by_gender(data, date.start, date.end, 'Prevalence', title=title ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, y.lim.max,0.05), limits=c(0,y.lim.max)) +
    ylab("HIV Prevalence (%)")
  return(p)

}

emodplot.incidence <- function(data,
                            date.start,
                            date.end) {
  data.incidence <- EMODAnalyzeR::calculate.incidence(data)
  y.lim.max <- min(max(data.incidence$incidence) * 1.2, 1)
  p <- emodplot.by_gender(data.incidence,date.start,date.end,'incidence') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,y.lim.max,0.005),limits=c(0,y.lim.max)) +
    ylab("HIV Incidence (%)")
  return(p)
}


emodplot.art <- function(data,
                     date.start,
                     date.end) {

  data$On_Art_scaled <- data$pop_scaling_factor * data$On_ART
  p <- emodplot.by_gender(data,date.start,date.end,'On_Art_scaled') +
    ylab("Number on Art")
  return(p)
}

#' Plot age-prevalence curves, stratified by year and gender
#' @description Plots age-prevalence curves using EMOD simulation outputs. Plots will show mean prevalence +/- 2 standard deviations.
#' @param data A tibble returned from read.simulation.results(..., stratify_columns = c("Year", "Age"," "Gender"), aggregate_columns = c("Population","Newly.Infected", "Infected")).
#' More aggregate columns can be used, but more stratify columns will cause problems in the plot.
#' @param subset_years list of years to plot
#' @param age_bins list of age bin boundaries, will be used to generate a vector of age bins formatted as "[age_0,age_1)", "[age_1,age_2)", etc
#' @param title Plot Title
#' @return ggplot of age-prevalence curves
emodplot.age_prevalence <- function(data,
                                    subset_years = c(2006, 2011, 2015),
                                    age_bins = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 99),
                                    title = ""){
  nScenarios = length(unique(data$scenario_name))
  if (nScenarios == 1) {
    colors2plot <- c('blue3')
  } else {
    colors2plot <- colors[1:nScenarios]
  }
  
  # Transform data
  data <- data %>% mutate(Gender = case_when(Gender==0 ~ "Male", Gender==1 ~ "Female"))
  # Subset data on years
  data <- data %>% filter(Year %in% subset_years)
  
  # Age bins
  age_labels = c()
  for (i in 1:(length(age_bins) - 1)){
    age_labels <- append(age_labels, paste0("[",age_bins[i],":",age_bins[i + 1],")"))
  }
  # Label each age by its bin
  data <- data %>% mutate(
    AgeBin = cut(Age,breaks = age_bins, right = FALSE)
    ) %>%
    filter(!is.na(AgeBin))
  
  data <- data %>% mutate(AgeBin_index = factor(AgeBin, 
                                                labels = 1:length(age_labels)))
  
  # Include prevalence
  data <- data %>% 
    group_by(Year, AgeBin_index, Gender, sim.id, scenario_name) %>% 
    summarize(Infected = sum(Infected), Population = sum(Population), .groups = 'keep') %>% 
    mutate(Prevalence = case_when(Population == 0 ~ 0,
                                  Population > 0 ~ Infected / Population))
  
  data.mean <- data %>%
    group_by(Year, AgeBin_index, Gender, scenario_name) %>%
    dplyr::summarise(mean.col2plot = mean(Prevalence), 
                     sd.col2plot = sd(Prevalence),
                     .groups = 'keep') %>% 
    mutate(lower = mean.col2plot - 2* sd.col2plot,
           upper = mean.col2plot + 2*sd.col2plot)
  
  # Plot
  p <- data.mean %>% 
    ggplot() + 
    # data
    geom_errorbar(
      mapping = aes(
        x=AgeBin_index, ymin=lower, ymax=upper,
        color=scenario_name),
      width=.25, size=1) + 
    # means
    geom_point(
      mapping = aes(
        x=AgeBin_index, y=mean.col2plot, 
        group=scenario_name, color=scenario_name), 
      size=1.5) + 
    facet_grid(cols = vars(Gender), rows = vars(Year)) + 
    xlab("Age") + 
    ylab("Prevalence") +
    theme_bw(base_size=16) +
    guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
    scale_x_discrete(
      breaks = 1:length(age_labels),
      labels = age_labels
      ) +
    theme(legend.position="bottom") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(strip.background = element_rect(colour="black", fill="white")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    scale_color_manual(values=colors2plot)  + 
    labs(title = title)
  
  return(p)
}