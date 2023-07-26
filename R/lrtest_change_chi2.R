## Function to evaluate Change in Likelihood ratio Chi2-statistic before and after adjustment for a covariate
## Function was built for stratified Cox regression analysis of the China Kadoorie Biobank data 



lrtest_change_chi2 <- function(dataset, 
                               main_expo_var,       # Charater vector of length 1
                               confounder_vars             = NULL,   # Character vector of names of confounding variables 
                               confounder_adjustment_label = NULL,   #Provide label (for example, " + Diabetes and BMI")
                               adjustment_vars             = NULL,   # Possible obtain Chi2 for the main exposure without a main confounder. Useful for minimally adjusted models 
                               stratify_vars               = NULL,   # Variable for stratified Cox regression 
                               minimal_adjustment          = FALSE,
                               print_model_info            = TRUE,   # Will print model output by default. 
                               futime                      = "lex.dur", # Follow-up time
                               event_var                   = "lex.Xst", # Events variable
                               cox_method                  = "breslow") {
  
  # Checks --
  if(length(main_expo_var) != 1) stop("`main_expo_var` must be a character vector of length 1")
  
  
  # Formula elements --
  left_side <- paste0("Surv(", futime, ", ", event_var, ")")
  
  stratify_vars <- stratify_vars[!stratify_vars %in% c(confounder_vars, adjustment_vars)] # Do not stratify by variables in `confounder_vars`
  strata_vars_c <- paste0("strata(", paste(stratify_vars, collapse = ", "),  ")")

  
  # Capture character covariates and convert them to factors before fitting the model --
  adjustment_vars <- adjustment_vars[!adjustment_vars %in% confounder_vars]
  
  get_char_list <- list()
  
  for (j in seq_along(adjustment_vars)) {
    
    if(class(dataset[[ adjustment_vars[j] ]]) == "character"){
      
      get_char_list[[ adjustment_vars[j] ]] <- adjustment_vars[j]  # Create a list of the character variables -- 
      
      }
    
  }
  
  
  if(length(get_char_list) > 0) {  
    
      # Remove list names and unlist--
      names(get_char_list) <- NULL
      get_char <- unlist(get_char_list)
      adjustment_vars <- adjustment_vars[!adjustment_vars %in% get_char] # Remove character vectors from `adjustment_vars`
      
      # Get adjustment vars for confounder/mediation model 
      adjust <- paste(
          paste(c(adjustment_vars), collapse = " + "),
          paste0(" + as.factor(", get_char, ") ", collapse = "") # Convert the character variables to factors  
      )
  
  }
  
  adjust <- paste(adjustment_vars, collapse = " + ") # If no character variables supplied, continue here
  
  
  # Fit model without main exposure variable --
  
  form0  <- as.formula(paste0(left_side, " ~ ", paste(adjust, strata_vars_c, sep = " + ")))
  m0     <- survival::coxph(form0, method = cox_method, data = dataset)
  
  if(print_model_info ==TRUE) {
    cat("Model formula with adjustment vars and no main exposure or confounder\n")
    print(form0)
    cat("Model summary is\n")
    print(summary(m0))
  }
  
  
  # Fit with main exposure --
  covs <- paste(main_expo_var, " + ", adjust)
  form1 <- as.formula(paste0(left_side, " ~ ", paste(covs, strata_vars_c, sep = " + ")))
  m1   <- survival::coxph(form1, method = cox_method, data = dataset)
  
  if(print_model_info ==TRUE) {
    cat("Model formula with main exposure and adjustment vars\n")
    print(form1)
    cat("Model summary is\n")
    print(summary(m1))
  }
  
  df_no_confounder <- # This is only return in case of no confounder adjustment (useful for "minimally adjusted models")
    broom::tidy(m1) %>% 
    dplyr::filter(term == main_expo_var) %>% 
    dplyr::select(term, estimate, std.error)
  
  
  ## Get Chi2 for exposure without confounder/mediator --
  lr_list1 <- lmtest::lrtest(m0, m1)
  
  lr_df1 <- data.frame(
      df_before  = lr_list1$Df[[2]],     #df = degrees of freedom
      chi2_before  = lr_list1$Chisq[[2]], 
      p_value_before = lr_list1$`Pr(>Chisq)`[[2]],
      confounder_adjust_before = "Before adjustment"
    )
  
  
  # PART 2 ----------
  # Isolate the Chi2 stats of the main exposure in the presence of the confounder
  # Fit with adjustment vars and main confounder  
  
  if(minimal_adjustment == FALSE) {
      get_conf_list <- list()
        for (c in seq_along(confounder_vars)) {
          if(class(dataset[[ confounder_vars[c] ]]) == "character"){ # Convert confounder to factor if character -- 
            get_conf_list[[ confounder_vars[c] ]] <- confounder_vars[c]
          }
        }
    
    
    if(length(get_conf_list) > 0) {  
        names(get_conf_list) <- NULL
        get_conf_char <- unlist(get_conf_list)
        confounder_vars <- confounder_vars[!confounder_vars %in% get_conf_char]
        
        confounder_vars <- paste(
            paste(c(confounder_vars), collapse = " + "),
            paste0(" + as.factor(", get_conf_char, ") ", collapse = "")
          )
  
        conf_adjust <- paste(adjust, " + ", confounder_vars)
    } 
    
    conf_adjust <- paste(adjust, " + ", confounder_vars)
    
    form2 <- as.formula(paste0(left_side, " ~ ", paste(conf_adjust, strata_vars_c, sep = " + ")))
    m2    <- coxph(form2, method = cox_method, data = dataset)
    
    if(print_model_info ==TRUE) {
      cat("Model formula with confounder and adjustment vars\n")
      print(form2)
      cat("Model summary is\n")
      print(summary(m2))
    }
    
    
    # Fit with main exposure, adjustment vars, and main confounder --
    main_conf_adjust <- paste(c(main_expo_var, conf_adjust), collapse = " + ")
    
    form3 <- as.formula(paste0(left_side, " ~ ", paste(main_conf_adjust, strata_vars_c, sep = " + ")))
    m3    <- survival::coxph(form3, method = cox_method, data = dataset)
    
    if(print_model_info ==TRUE) {
      cat("Model formula with main exposure, confounder and adjustment vars (M3)\n")
      print(form3)
      cat("Model summary is\n")
      print(summary(m3))
      }
    
    df_confounder <- 
      broom::tidy(m3) %>% 
      dplyr::filter(term == main_expo_var) %>% 
      dplyr::select(term, estimate, std.error)
    
    ## Get CHi2 for exposure without confounder 
    lr_list2 <- lmtest::lrtest(m2, m3)
    
    lr_df2   <- data.frame(df_after          = lr_list2$Df[[2]], 
                           chi2_after        = lr_list2$Chisq[[2]], 
                           p_value_after     = lr_list2$`Pr(>Chisq)`[[2]],
                           confounder_adjust = paste0("After adjustment for ", paste0(confounder_vars, collapse = ", "))
                           )
    
  }
  
  # Bind dataframes -------
 
   if(is.null(confounder_vars) == FALSE) {
      lr_df <- cbind(df_confounder, lr_df1, lr_df2) 
      lr_df$chi_change <- lr_df$chi2_before -  lr_df$chi2_after 
    } else  {
      lr_df <- cbind(df_no_confounder, lr_df1)  
      lr_df$adjustment <- paste0(adjustment_vars, collapse = ", ")
    }  
  
  if( !is.null(confounder_adjustment_label) ){
    lr_df$adjust_label <- confounder_adjustment_label
  }
  
  
  # Results --------
  
  return(as.data.frame(lr_df))
  
  
}
