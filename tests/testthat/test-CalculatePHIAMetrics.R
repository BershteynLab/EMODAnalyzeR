test_that("read.ingest.file works", {
  phia_data = readRDS("../test_data/TestZamphiaRead.RDS")
  ci = calculate.phia_survey.prevalence(phia_data, age_min_inclusive = 15, age_max_inclusive = 49)
  ci_from_phia_site = data.frame(prevalence=c(0.114,0.143,0.083),
                                 lb=        c(0.107,0.134,0.076),
                                 ub=        c(0.120,0.151,0.090),
                                 gender=c("all","female","male"))
  ci[,c("prevalence","lb","ub")] = round(ci[,c("prevalence","lb","ub")],3)
  expect_equal(ci, ci_from_phia_site)
})
