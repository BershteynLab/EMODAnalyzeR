calculate.phia_survey.prevalence <- function(phia_data, age_min_inclusive, age_max_inclusive, regions=NULL ) {
  phia_data = phia_data %>%
              filter(age >= age_min_inclusive,
                     age <= age_max_inclusive,
                     hivstatusfinal != 99,
                     bt_status==1)
  design = svrepdesign(NULL, "btwt[0-9]\\d\\d",
                       scale=1, rscales=1,
                       weights=~btwt0, type="JKn",
                       data=phia_data,
                       df=25)
  design_female = svrepdesign(NULL, "btwt[0-9]\\d\\d",
                       scale=1, rscales=1,
                       weights=~btwt0, type="JKn",
                       data=phia_data %>% filter(gender == 2),
                       df=25)
  design_male = svrepdesign(NULL, "btwt[0-9]\\d\\d",
                       scale=1, rscales=1,
                       weights=~btwt0, type="JKn",
                       data=phia_data %>% filter(gender == 1),
                       df=25)

  get_ci_from_design = function(design) {
    ci = svyciprop(~I(hivstatusfinal==1),design, method = "mean",df=25)
    data.frame(prevalence=ci[[1]],
               lb=attributes(ci)$ci[["2.5%"]],
               ub=attributes(ci)$ci[["97.5%"]])
  }

  df_total = get_ci_from_design(design)
  df_total$gender <- "all"
  df_female = get_ci_from_design(design_female)
  df_female$gender <- "female"
  df_male = get_ci_from_design(design_male)
  df_male$gender <- "male"
  rbind(df_total, df_female, df_male)
}



calculate.phia_survey.effective_count = function(input) {
  alpha_optimized <- seq(nrow(input),1)
  beta_optimized  <- seq(nrow(input),1)
  effective_count <- seq(nrow(input),1)

  lower_fitted    <- seq(nrow(input),1)
  upper_fitted    <- seq(nrow(input),1)
  mean_fitted     <- seq(nrow(input),1)

  for(row_iter in seq(1,nrow(input))){

    alpha_initial <- 10000

    to_minimize <- function(alpha){
      alpha <- abs(alpha)
      beta_computed <- -alpha + ( (alpha)/input[row_iter,'prevalence'] ) #alpha*((1-input[row_iter,'prevalence'])/input[row_iter,'prevalence'])
      #        print(alpha/(alpha+beta_computed))
      penalty <- (input[row_iter,'lb'] - qbeta(0.025, alpha, beta_computed,  ncp = 0, lower.tail = TRUE, log.p = FALSE))^2  +
        (input[row_iter,'ub'] - qbeta(0.975, alpha, beta_computed,  ncp = 0, lower.tail = TRUE, log.p = FALSE))^2
      return(penalty)
    }

    to_minimize(alpha_initial)

    alpha_optimized[row_iter] = optim(alpha_initial, to_minimize, method="Brent", lower=0, upper=1e6, control = list(abstol = 0.00000001))$par
    beta_optimized[row_iter]  <- -alpha_optimized[row_iter]  + ( (alpha_optimized[row_iter])/input[row_iter,'prevalence'] ) #-alpha + ( (alpha)/input[row_iter,'prevalence'] )

    lower_fitted[row_iter]    = qbeta(0.025, alpha_optimized[row_iter], beta_optimized[row_iter], ncp = 0, lower.tail = TRUE, log.p = FALSE)
    upper_fitted[row_iter]    = qbeta(0.975, alpha_optimized[row_iter], beta_optimized[row_iter], ncp = 0, lower.tail = TRUE, log.p = FALSE)
    mean_fitted[row_iter]     = (alpha_optimized[row_iter])/(alpha_optimized[row_iter] + beta_optimized[row_iter])


    effective_count[row_iter] = alpha_optimized[row_iter] + beta_optimized[row_iter] - 1

    print(paste(input$Year,input$Province, input$Gender, input$AgeBin, "alpha=",alpha_optimized[row_iter], "beta=",beta_optimized[row_iter],sep=" "))
    print(paste("Was aiming for mean of ", input[row_iter,'prevalence'], "and got",   mean_fitted[row_iter] ))
    print(paste("Was aiming 2.5%ile of ", input[row_iter,'lb'], "and got",  lower_fitted[row_iter] ))
    print(paste("Was aiming 97.5%ile of", input[row_iter,'ub'], "and got",  upper_fitted[row_iter] ))

  }

  input$effective_count <- effective_count

  input$alpha <- alpha_optimized
  input$beta <- beta_optimized
  input$mean_fitted  <- mean_fitted
  input$lower_fitted <- lower_fitted
  input$upper_fitted <- upper_fitted

  input$pct_diff_mean   <- (input$mean_fitted-input$prevalence)/input$mean_fitted
  input$pct_diff_lower  <- (input$lower_fitted-input$lb)/input$lower_fitted
  input$pct_diff_upper  <- (input$upper_fitted-input$upper)/input$upper_fitted
  input
}
