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
    xlab("Year") +
    xlim(c(date.start, date.end)) +
    ylab(paste0(col2plot, " ", unit)) +
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
  emodplot.by_gender(data, date.start, date.end, 'Prevalence', title=title ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, y.lim.max,0.05), limits=c(0,y.lim.max)) +
    ylab("HIV Prevalence (%)")

}

emodplot.artcoverage <- function(data,
                                date.start,
                                date.end,
                                title = "ART Coverage",
                                node_id = "All") {
  if (str_to_lower(node_id) != "all") {
    data <- data %>% filter(NodeId == node_id)
  }
  data <- data %>%
    group_by(Year, Gender, scenario_name, sim.id) %>%
    summarize(On_ART = sum(On_ART), Infected = sum(Infected))
  data <- data %>% filter(Infected > 0) %>% mutate(art_coverage = On_ART / Infected )
  y.lim.max <- min(max(data$art_coverage) * 1.2, 1.0)
  emodplot.by_gender(data, date.start, date.end, 'art_coverage', title=title ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, y.lim.max,0.05), limits=c(0,y.lim.max)) +
    ylab("ART Coverage (% of Infected)")

}


emodplot.incidence <- function(data,
                            date.start,
                            date.end) {
  data.incidence <- EMODAnalyzeR::calculate.incidence(data)
  y.lim.max <- min(max(data.incidence$incidence) * 1.2, 1)
  emodplot.by_gender(data.incidence,date.start,date.end,'incidence') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,y.lim.max,0.005),limits=c(0,y.lim.max)) +
    ylab("HIV Incidence (%)")
}


emodplot.art <- function(data,
                     date.start,
                     date.end) {

  data$On_Art_scaled <- data$pop_scaling_factor * data$On_ART
  emodplot.by_gender(data,date.start,date.end,'On_Art_scaled') +
    ylab("Number on Art")
}


