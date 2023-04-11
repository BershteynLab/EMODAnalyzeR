test_that("read.simulation.results works", {
  read.simulation.results("../test_data/ReportHIVByAgeAndGender",scenario_name = "test")
  expect_true(TRUE)
})

test_that("read.ingest.file works", {
  read.ingest.file("../test_data/calibration_ingest_form_Nyanza.xlsm")
  expect_true(TRUE)
})
