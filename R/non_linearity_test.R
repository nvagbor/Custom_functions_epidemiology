# Function to automate test for departure from linearity for numeric and ordered vectors

# External dependency: broom package

# TO DO: Format the right hand side of the formula to read nicely when covariate option is NULL

lrtest_non_linearity <- function(data, 
                                test_var,                       # Variable to test for non-linearity
                                adjust_var  = NULL,             # Variables to adjust for
                                futime      = "lex.dur",
                                event_var   = "lex.Xst",
                                cox_method  = "breslow",
                                strata_vars){                    
  
  if(missing(strata_vars) || is.null(strata_vars)) {
    
    stop("Stratifying variable(s) must be provided")
    
 }
 
  
  strata_vars_c <- paste0( "strata(", paste(strata_vars, collapse = ", "), ")" )
  left_side <- paste0("Surv(", futime, ", ", event_var, ")")
  covs      <- paste(c(adjust_var), collapse = " + ")
  
  # Fit model without variable of interest
  form <- as.formula(paste0(left_side, " ~ ", paste(covs, strata_vars_c, sep = " + ")))
  m0   <- coxph(form, method = cox_method, data = data)
  
  
  # For continuous exposure ---------------------------------------------------------------------------------------------------
  if(is.numeric(data[[test_var]]) || is.integer(data[[test_var]])) {
    
    # Create quadratic and cubic terms
    var_quad  <- paste0(test_var, "_quad")
    var_cubic <- paste0(test_var, "_cubic")
    
    data[ ,var_quad]  <- data[[test_var]]^2
    data[ ,var_cubic] <- data[[test_var]]^3
    
    
    # Test for trend  
    covs1  <- paste(covs, test_var, sep = " + ")
    form1  <- as.formula(paste0(left_side, " ~ ", paste(covs1, strata_vars_c, sep = " + ")))
    m1     <- coxph(form1, method = cox_method, data = data)
    
    trend    <- lrtest(m0, m1)
    trend_df <- data.frame(DF        = trend$Df[[2]], 
                           Chi2      = trend$Chisq[[2]], 
                           p_value   = trend$`Pr(>Chisq)`[[2]], 
                           power     = 1, 
                           test_type = "Linear trend",
                           exposure  = test_var
                           )
    
    # Introduce quadratic terms (departure from linearity)
    covs2 <- paste(covs1, var_quad, sep = " + ")
    form2 <- as.formula(paste0(left_side, " ~ ", paste(covs2, strata_vars_c, sep = " + ")))
    m2    <- coxph(form2, method = cox_method, data = data)
    
    depart1    <- lrtest(m1, m2)
    depart1_df <- data.frame(DF        = depart1$Df[[2]],
                             Chi2      = depart1$Chisq[[2]],
                             p_value   = depart1$`Pr(>Chisq)`[[2]],
                             power     = 2,
                             test_type = "Quadratic term",
                             exposure  = test_var
                            )
    
    # Introduce quadratic terms (departure from linearity)
    covs3 <- paste(covs2, var_cubic, sep = " + ")
    form3 <- as.formula(paste0(left_side, " ~ ", paste(covs3, strata_vars_c, sep = " + ")))
    m3    <- coxph(form3, method = cox_method, data = data)
    
    depart2    <- lrtest(m2, m3)
    depart2_df <- data.frame(DF        = depart2$Df[[2]],
                             Chi2      = depart2$Chisq[[2]],
                             p_value   = depart2$`Pr(>Chisq)`[[2]],
                             power     = 3,
                             test_type = "Cubic term",
                             exposure  = test_var
                             )
    
    # Create list of results
    list_name  <- c("Linear", "Quadratic", "Cubic")
    model_info <- list(trend, depart1, depart2)
    names(model_info) <- list_name
    
    test_df    <- rbind(trend_df, depart1_df, depart2_df)
    
    results    <- list(model_info, test_df)
    
    return(results)
    
  }
  
  
  
  # For ordered factors (nominal variables) --------------------------------------------------------------------------------------------------
  if(is.factor(data[[test_var]])){
    
    data[ , test_var] <- factor(data[[test_var]], ordered = TRUE)
    
    # Fit model with ordered factor 
    covs1  <- paste(test_var, covs, sep = " + ")
    form1  <- as.formula(paste(left_side, " ~ ", paste(covs1, strata_vars_c, sep = " + "), collapse = " "))
    m1     <- coxph(form1, method = cox_method, data = data)
    
    model_sum <- summary(m1)
    model_df  <- broom::tidy(m1) 
    
    # Wrangling
    arbit_name <- c(
          "Linear", 
          "Quadratic", 
          "Cubic", 
          "Quartic", 
          "Quintic", 
          "Sextic", 
          "Septic", 
          "Octic", 
          "Nonic")
    
    test_type_name     <- arbit_name[c(1: (length(levels(data[[test_var]])) - 1) )]
    
    model_df$test_type <- NA
    model_df$test_type[c(1:length(test_type_name))] <- test_type_name
    
    # Select relevant columns
    # The coefficients do not have any meaningful interpretation
    model_df_reduced   <- model_df [ , c("test_type", "term", "p.value")]
    results            <- list(model_sum, model_df_reduced)
    
    return(results)
    
  }
  
  
}

