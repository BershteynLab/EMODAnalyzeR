library(ggplot2)

colors = c("#0072b2","#009e73","#cc79a7")

plot.by_gender <- function(data,
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
    dplyr::summarise(mean(col2plot))
  ggplot(data=subset(data, (date.start <= Year) & (Year <= date.end) ) ) +
    geom_point(size=1.0, aes(x=Year, y=col2plot*1, group=sim.id, color=scenario_name), alpha=0.005)+
    geom_line(data=subset(data.mean, (date.start <= Year) & (Year <= date.end)), aes(x=Year, y=`mean(col2plot)`*1, group=scenario_name, color=scenario_name), size=1.5) +
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

plot.prevalence <- function(data,
                           date.start,
                           date.end,
                           title = "HIV prevalence") {
  data <- data %>% mutate(prevalence = Infected / Population)
  y.lim.max <- max(data$prevalence) * 1.2
  plot.by_gender(data, date.start, date.end, 'prevalence', title=title ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, y.lim.max,0.05), limits=c(0,y.lim.max)) +
    ylab("HIV Prevalence (%)")

}

plot.incidence <- function(data,
                            date.start,
                            date.end) {
  incidence <- calculate.incidence(data)
  plot.by_gender(incidence,2000,2050,'incidence') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.038,0.01),limits=c(0,0.038))

}





