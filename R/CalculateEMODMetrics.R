calculate.incidence<- function(data) {
  data$Year_Integer <- floor((data$Year-0.5))

  trajectories_IR.1a <- aggregate(Newly.Infected ~ Year_Integer+Gender+sim.id+scenario_name, data=data,FUN=sum) #sum number of new infections in each year
  print(head(trajectories_IR.1a, 10))

  #Make the denominator as HIV-negative individuals
  trajectories_IR.2 <- aggregate(Population - Infected ~ Year+Gender+sim.id+scenario_name, data=data, FUN=sum)
  head(trajectories_IR.2,10)

  trajectories_IR.2$Year_Integer <- floor(trajectories_IR.2$Year-0.5)

  trajectories_IR.2 <- trajectories_IR.2[!duplicated(trajectories_IR.2[c("Year_Integer","Gender","sim.id","scenario_name")]),] #remove second instance of duplicate rows

  trajectories_IR.2 <- trajectories_IR.2[-match("Year",names(trajectories_IR.2))]

  trajectories_IRoverall <- merge(trajectories_IR.1a, trajectories_IR.2, by=c("Year_Integer","Gender","sim.id","scenario_name"))

  trajectories_IRoverall$incidence <- trajectories_IRoverall$Newly.Infected / (trajectories_IRoverall$Population-(trajectories_IRoverall$Newly.Infected/2))
  trajectories_IRoverall %>% dplyr::rename(Year = Year_Integer)

}
