test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("read.simulation.results works", {
  read.simulation.results("../test_data/ReportHIVByAgeAndGender",scenario_name = "test")
  expect_true(TRUE)
})


test_that("dalys are consistant across different stratifications", {
  sim_1 = read.simulation.results("../test_data/ReportHIVByAgeAndGender", scenario_name = "test", stratify_columns = c("Year","Age"))
  sim_1 = calculate.pop_scaling_factor(sim_1, 2010, 1000000)
  sim_2 = read.simulation.results("../test_data/ReportHIVByAgeAndGender", scenario_name = "test", stratify_columns = c("Year","Age","Gender"))
  sim_2 = calculate.pop_scaling_factor(sim_2, 2010, 1000000)
  daly_1 = calculate.DALY(sim_1)
  daly_2= calculate.DALY(sim_2)
  expect_equal(daly_1, daly_2)
})


test_that("simple daly calculation", {
  Year = seq(2000,2004)
  Age = rep(50,5)
  Died_from_HIV = c(0,1,2,0,0)
  On_ART = c(0,1,0,1,0)
  Infected = c(1,2,2,1,1)
  sim = data.frame(Year,Age,Died_from_HIV,On_ART, Infected)
  sim$pop_scaling_factor = 1
  sim$sim.id = "test"
  sim$scenario_name = "testtest"
  dalys = calculate.DALY(sim, infected_weight = 0.1,discount_percent = 0, art_weight = 0.01)

  expect_equal(sum(dalys$yll),10)
  expect_equal(sum(dalys$daly), 10. + 0.1*5 + 0.01*2)
})
