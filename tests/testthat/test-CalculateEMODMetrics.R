

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

test_that("calculate.incidence correctly calculates one person per gender getting infected each half-year", {
  Year = seq(2000.5,2004,0.5)
  Population = rep(100, length(Year)) + seq(length(Year))
  Infected = seq(length(Year))
  Newly.Infected = rep(1, length(Year))
  Gender = rep(0, length(Year))
  df_men = data.frame(Year, Gender, Population, Infected, Newly.Infected)
  df_women = df_men %>% mutate(Gender = 1)
  df = rbind(df_men, df_women) %>% mutate(sim.id=1, scenario_name="test")
  incidence = df %>% calculate.incidence() %>% mutate(error = abs(incidence - 0.02))
  expect_true(all(incidence$error == 0))
})

test_that("calculate.incidence correctly calculates two people (both men+women) getting infected each half-year", {
  Year = seq(2000.5,2004,0.5)
  Population = rep(100, length(Year)) + seq(length(Year))
  Infected = seq(length(Year))
  Newly.Infected = rep(1, length(Year))
  Gender = rep(0, length(Year))
  df_men = data.frame(Year, Gender, Population, Infected, Newly.Infected)
  df_women = df_men %>% mutate(Gender = 1)
  df = rbind(df_men, df_women) %>% mutate(sim.id=1, scenario_name="test")
  incidence = df %>% calculate.incidence(gender.breakdown = FALSE) %>% mutate(error = abs(incidence - 0.02))
  expect_true(all(incidence$error == 0))
})


test_that("calculate.prevalence correctly calculates two people (both men+women) getting infected each half-year", {
  Year = seq(2000.5,2004,0.5)
  Newly.Infected = rep(1, length(Year))
  Population = rep(100, length(Year))
  Infected = seq(length(Year))
  Gender = rep(0, length(Year))
  correct_prevalence = rep(c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08),2)
  df_men = data.frame(Year, Gender, Population, Infected, Newly.Infected)
  df_women = df_men %>% mutate(Gender = 1)
  df = rbind(df_men, df_women) %>% mutate(sim.id=1, scenario_name="test")
  prevalence = df %>% calculate.prevalence() %>% arrange(Gender)
  expect_true(all(prevalence$Prevalence == correct_prevalence))
})

test_that("calculate.tests.performed correctly calculates 25% positive proportions", {
  Year = seq(2000.5,2004,0.5)
  Newly.Tested.Positive = rep(25, length(Year))
  Newly.Tested.Negative = rep(75, length(Year))
  Gender = rep(0, length(Year))
  df_men = data.frame(Year, Gender, Newly.Tested.Positive, Newly.Tested.Negative)
  df_women = df_men %>% mutate(Gender = 1)
  df = rbind(df_men, df_women) %>% mutate(sim.id=1, scenario_name="test")
  tests = df %>% calculate.tests.performed()
  correct_proportion = rep(0.25,nrow(df)/2) # divided by 4 because we aggregate gender and 
  expect_true(all(tests$Proportion.Positive.Tests == correct_proportion))
})


