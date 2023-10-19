test_that("calculate.phia_survey.prevalence matches PHIA website", {
  phia_data = readRDS("../test_data/TestZamphiaRead.RDS")
  ci = calculate.phia_survey.prevalence(phia_data, age_min_inclusive = 15, age_max_inclusive = 49)
  ci_from_phia_site = data.frame(prevalence=c(0.114,0.143,0.083),
                                 lb=        c(0.107,0.134,0.076),
                                 ub=        c(0.120,0.151,0.090),
                                 gender=c("all","female","male"))
  ci[,c("prevalence","lb","ub")] = round(ci[,c("prevalence","lb","ub")],3)
  expect_equal(ci, ci_from_phia_site)
})

test_that("calculate.phia_survey.effective_count can calculate inverse of qbeta", {
  n = 100
  set.seed(1993)
  ci_from_phia_site = data.frame(prevalence=c(0.114,0.143,0.083),
                                 gender=c("all","female","male")) %>%
                        mutate(alpha = prevalence*n, beta=(1-prevalence)*n) %>%
                        mutate(lb = qbeta(0.025, alpha, beta)) %>%
                        mutate(ub = qbeta(0.975, alpha, beta))
  effective_counts = calculate.phia_survey.effective_count(ci_from_phia_site )
  error = abs(effective_counts$effective_count-n)
  expect_true(all(error < 0.0001))
})


test_that("calculate.tests.performed ")
