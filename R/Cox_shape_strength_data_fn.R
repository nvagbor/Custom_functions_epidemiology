
# Function fits a stratified Cox reg model 
# and computes FAR or correct for regression dilution depending on the needs of the user
# Will get the number of events from the dataset  supplied
# Depreciated some arguments like: `exposure_var_sd`
# Packages used in the function: Tidyverse, broom, survival, survminer, and qvcalc


Cox_shape_strength_data <- function(
    dataset,
    exposure_var_cat            = NULL,        # Exposure is categorical (supply exposure name as a character vector)
    get_FAR                     = TRUE,
    exposure_var_cont           = NULL,        # Exposure is numeric (supply exposure name as a character vector)
    expo_type_shape             = FALSE,       # Need to get FAR to make shape plot 
    expo_type_continuous        = FALSE,      
    correct_regression_dilution = FALSE,        
    regression_dilution_ratio   = NULL,        # Numeric vector of length 1
    print_model_info            = TRUE,
    event_var                   = "lex.Xst",
    fu.time                     = "lex.dur",
    cox_method                  = "breslow",
    stratify_vars, 
    adjustment_vars             = c("age")) 
    {
  
    # Create formula for stratified Cox analysis
    left_side <- paste0("Surv(", fu.time, ", ", event_var, ")")

    # Fit a stratified model? 
    if(!is.missing(stratify_vars)){
      strata_vars <- paste0("strata(",  paste(stratify_vars, collapse = ", "),  ")" )
      adjustment_vars <- adjustment_vars[!adjustment_vars %in% stratify_vars]   # Do not adjust for stratifying vars
      }
    
    # Get FAR for categorical expo for shape plot ----
    if(expo_type_shape==TRUE) {
      # Check if make exposure is a factor and convert if not
      if(!is.factor(dataset[[exposure_var_cat]])){dataset[ ,exposure_var_cat] <- as.factor(dataset[[exposure_var_cat]])}
      
      # Make formula
      covariates <- paste(c(exposure_var_cat, adjustment_vars), collapse=" + ")
      if(!is.missing(stratify_vars){
        form <- as.formula(paste0(left_side, " ~ ", paste(covariates, strata_vars, sep=" + ") ))
        }else{
          form <- as.formula(paste0(left_side, " ~ ", covariates))
        }
      
      # Length of factor levels
      expo_var_lev <- levels(dataset[[exposure_var_cat]])
      n_levels_exp <- length(levels(dataset[[exposure_var_cat]]))
      
      # Events (N)
      n_events <- dplyr::reframe(dataset, n_events=sum(.data[[event_var]]), by=exposure_var_cat) %>% pull(n_events)
      
      # Fit model 
      model_fn   <- survival::coxph(formula = form, method = cox_method, data = dataset) 
      model_summ <- summary(model_fn)
      if(print_model_info==TRUE) {print(model_summ) }
      
      model_df   <- 
        broom::tidy(model_fn) %>% 
        dplyr::filter(grepl(pattern=exposure_var_cat, term)) %>% 
        dplyr::add_row(.before=1) %>% 
        dplyr::transmute(
          exposure=expo_var_lev, 
          n=n_events,
          estimate=ifelse(is.na(estimate), 0, estimate), 
          std.error=std.error, 
          p.value=p.value)
      
      if(get_FAR==TRUE){
        FAR <- 
        qvcalc::qvcalc(model_fn, exposure_var_cat)$qvframe %>% 
        dplyr::transmute(quasiSE=quasiSE, quasiVar=quasiVar) %>% 
        `row.names<-`(NULL) 
        
        df_far <- cbind(model_df, FAR) 
        
      } else{df_far <- model_df}
      
      return(list("model_object"=model_fn, "dataframe"=df_far)) 
         
    } 
    
    # Continuous exposure ---------
    
         ## Correct for regression dilution 
    
    if(expo_type_continuous==TRUE && correct_regression_dilution==TRUE) {
      if(all( is.null(exposure_var_cont) | is.null(regression_dilution_ratio) )) {
        
        stop("`exposure_var_cont` and `regression_dilution_ratio` must be provided\n")
      }
      
      if(!is.null(exposure_var_cat)){ 
        stop("`exposure_var_cat` must be set to NULL if `expo_type_continuous` == TRUE\n")}
      
      if(!is.null(exposure_var_sd)){ 
        stop("`exposure_var_sd` must be set to NULL if `expo_type_continuous` == TRUE\n")}
      
      # Make formula
      covariates   <- paste(c(exposure_var_cont,adjustment_vars), collapse = " + ")

      if(!is.missing(stratify_vars){
        form <- as.formula(paste0(left_side, " ~ ", paste(covariates, strata_vars, sep=" + ")))
        }else{
        form <- as.formula(paste0(left_side, " ~ ", covariates))
      }
      
      model_cont <- coxph(form, method=cox_method, data=dataset)
      model_cont_summ <- summary(model_cont)
      if(print_model_info==TRUE) {print(model_cont_summ) }
      
      model_df  <- 
        broom::tidy(model_cont) %>% 
        dplyr::filter(grepl(pattern=exposure_var_cont, term)) %>% 
        dplyr::mutate(
                est_corrected=estimate/regression_dilution_ratio,    # Correct estimate/se for regression dilution
                se_corrected=std.error/regression_dilution_ratio,
                hr_corrected=exp(est_corrected), 
                lci_corrected=exp(est_corrected-1.96*se_corrected), 
                uci_corrected=exp(est_corrected+1.96*se_corrected)) %>% 
        dplyr::select(-statistic)
      
      return(list("model_object"=model_cont, "dataframe"=model_df)) 
      
    }

         ## DO NOT correct for regression dilution 
    
    if(expo_type_continuous==TRUE  && correct_regression_dilution==FALSE) {
      
      # Make formula
      covariates   <- paste(c(exposure_var_cont,adjustment_vars), collapse = " + ")
      if(!is.missing(stratify_vars){
        form <- as.formula(paste0(left_side, " ~ ", paste(covariates, strata_vars, sep=" + ")))
        }else{
        form <- as.formula(paste0(left_side, " ~ ", covariates))
        }
      
      model_cont      <- survival::coxph(form, method=cox_method, data=dataset)
      model_cont_summ <- summary(model_cont)
      if(print_model_info==TRUE) {print(model_cont_summ) }
      
      model_df  <- 
        broom::tidy(model_cont) %>% 
        dplyr::filter(grepl(pattern=exposure_var_cont, term)) %>% 
        dplyr::mutate(hr=exp(estimate), 
                      lci=exp(estimate-1.96*std.error), 
                      uci=exp(estimate+1.96*std.error)) %>% 
        dplyr::select(-statistic)
      
      return(list("model_object"=model_cont, "dataframe"=model_df))
      
    }    
    
  }




