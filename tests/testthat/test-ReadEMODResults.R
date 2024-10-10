test_that("read.simulation.results runs", {
  df = read.simulation.results("../test_data/ReportHIVByAgeAndGender",scenario_name = "test")
  expect_true(TRUE)
})

test_that("read.ingest.file works", {
  read.ingest.file("../test_data/calibration_ingest_form_Nyanza.xlsm")
  expect_true(TRUE)
})

test_that("read.simulation.results.bigpurple works", {
  df = read.simulation.results.bigpurple("../test_data/BigPurpleFS", "Test Scenario" )
  expect_true(nrow(unique(df$sim.ix)) == 2)
})
