## Assess if a variable improves model fit using LR test 
## Function fits a stratified cox regression model 

## Dependencies: lmtest package, survminer

## Value: list and dataframe of LR test 


lrtest_model_fit <- function(data, 
                             test_var, 
                             adjustment_vars = NULL, 
                             strata_vars     = NULL, 
                             futime          = "lex.dur", 
                             event_var       = "lex.Xst"){
  
  if(missing(strata_vars) || is.null(strata_vars)) {
    
    stop("Stratifying variable(s) must be provided")
    
  }
  
  
  left_side     <- paste0("Surv(", futime, ", ", event_var, ")")
  
  strata_vars_c <- paste0("strata(", 
                          paste(strata_vars, collapse = ", "), 
                          ")"
                         )
  
  adjust        <- paste(c(adjustment_vars), collapse = " + ")
  
  # Fit nested model 
  form <- as.formula(paste0(left_side, " ~ ", paste(adjust, strata_vars_c, sep = " + ")))
  m0   <- coxph(form, data = data)
  
  # Fit full model 
  covs <- paste(c(test_var, adjustment_vars), collapse = " + ")
  
  form <- as.formula(paste0(left_side, " ~ ", paste(covs, strata_vars_c, sep = " + ")))
  m1   <- coxph(form, data = data)
  
  # Assess improvement in model fit using lrtest
  lr_list <- lmtest::lrtest(m0, m1)
  
  # Wrangling 
  lr_df <- data.frame(DF        = lr_list$Df[[2]], 
                      Chi2      = lr_list$Chisq[[2]], 
                      p_value   = lr_list$`Pr(>Chisq)`[[2]],
                      exposure  = test_var
                      )
  
  results <- list(lr_list, lr_df)
  
  # Return value
  return(results)
} 


