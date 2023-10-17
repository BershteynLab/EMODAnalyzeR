
test_that("emodplot.prevalence works", {
  df = read.simulation.results("../test_data/ReportHIVByAgeAndGender",scenario_name = "test")
  emodplot.prevalence(df, 2000, 2020 )
  expect_true(TRUE)
})


test_that("emodplot.artcoverage works", {
  df = read.simulation.results("../test_data/ReportHIVByAgeAndGender",scenario_name = "test")
  emodplot.artcoverage(df, 2000, 2020 )
  expect_true(TRUE)
})

test_that("emodplot.incidence works", {
  df = read.simulation.results("../test_data/ReportHIVByAgeAndGender",scenario_name = "test")
  emodplot.incidence(df, 2000, 2020 )
  expect_true(TRUE)
})


test_that("emodplot.age_prevalence works", {
  df = read.simulation.results("../test_data/ReportHIVByAgeAndGender",scenario_name = "test", stratify_columns = c("Year", "Gender", "Age"))
  agebins =c(15,20,25,30,35,40,45,50)
  emodplot.age_prevalence(df,age_bins = agebins )
  expect_true(TRUE)
})
