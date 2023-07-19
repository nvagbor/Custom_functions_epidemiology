# Fit interaction terms in a stratified Cox regression model


Cox_interaction_data <- 
  
  function(
    
    dataset,
    main_exposure_var,                         # Character vector of length 1. Only accepts numeric exposures
    nominal_interaction_vars    = NULL,           # An LR test for heterogeneity will be performed for interaction 
    nominal_labels              = NULL,           # Vector of labels to be used for nominal variables. e.g., c("Age, years")
    ordinal_interaction_vars    = NULL,           # An LR test for trend will be performed for interaction 
    ordinal_labels              = NULL,
    multiple_interaction_vars   = NULL,        # To stratify by more than one subgroup
    test_interaction            = TRUE,        # Perform test for interaction by default
    correct_regression_dilution = FALSE,        
    regression_dilution_ratio   = NULL,        # Numeric vector of length 1 
    print_model_info            = FALSE,
    event_var                   = "lex.Xst",
    fu.time                     = "lex.dur",
    cox_method                  = "breslow",
    stratify_vars               = c("age_lexis5", "sex","region"),  # Function will not stratify by variables used for subgroup analysis 
    adjustment_vars             = c("age")   # Function will not adjust for variables used for subgroup analysis 
    
  ) {
    
  # Load function for interaction tests -- 
    
    trend <- function(beta, se) {
      
      # beta must be a vector of estimates
      # se must be a vector of their standard errors
      
      if (length(beta) != length(se)) {
        stop("Length of vector of estimates must be equal to length of vector of standard errors\n")
      }
      
      z    <- seq(1:length(beta))
      w    <- 1 / (se^2)
      test <- (sum(w * beta *(z - (sum(z * w) / sum(w))))^2) / sum(w * ((z - sum(z * w) / sum(w))^2))
      p    <- pchisq(test, df = 1, lower = FALSE)
      
      return(list("df" = 1, "test statistic" = test, "p" = p))
    }
    
    
    heterogeneity <- function(beta, se) {
      
      # degrees of freedom	
      df <- length(beta) - 1
      
      # expected_beta: inverse variance weighted average of betas
      expected_beta <- sum(beta / se^2) / sum(1 / se^2)
      
      heterogeneity_test_statistic <- sum(((beta - expected_beta) / se)^2)
      
      p <- pchisq(heterogeneity_test_statistic, df = df, lower = FALSE)
      
      return(list("df" = df, "test statistic" = heterogeneity_test_statistic, "p" = p))
      
    }
    
    
    
  # 1. Check class of main exposure ---
      
    ## Exposure must be numeric 
    ## Interaction or subgroup variables must be categorical 
      
    stopifnot(any( class(dataset[[main_exposure_var]])  == "numeric" | class(dataset[[main_exposure_var]])  == "integer") )
    
    
    ## Conditions for nominal and ordinal variables --
    
    if(!is.null(nominal_labels)){
      
      if(is.null(nominal_interaction_vars)) 
        stop("`nominal_interaction_vars` must be provided\n")
      
      if(length(nominal_labels) != length(nominal_interaction_vars)) 
        stop("Length of `nominal_labels` and `nominal_interaction_vars` must be equal\n")
    
      }
  
    
    if(!is.null(ordinal_labels)){
      
      if(is.null(ordinal_interaction_vars)) stop("`ordinal_interaction_vars` must be provided\n")
      
      if(length(ordinal_labels) != length(ordinal_interaction_vars)) 
        stop("Length of `ordinal_labels` and `ordinal_interaction_vars` must be equal\n")
      
    }
    
    
    ## Conditions to correct for regression dilution bias --
    
    if(correct_regression_dilution == TRUE){
      
      if(is.null(regression_dilution_ratio)) stop("`regression_dilution_ratio` must be provided\n")
      if(length(regression_dilution_ratio) != 1) stop("`regression_dilution_ratio` must be of length 1\n")
    
    }
    
    
    ## Conditions to run multiple interaction --
    
    if(!is.null(multiple_interaction_vars) == TRUE){ 
      
      # Multiple interaction analysis should be conducted separately 
      if(!is.null(nominal_interaction_vars) == TRUE)
        stop("The `nominal_interaction_vars` and `ordinal_interaction_vars` must be set\nto NULL to perform multiple interaction")
      
      
      if(!is.null(ordinal_interaction_vars) == TRUE)
        stop("The `nominal_interaction_vars` and `ordinal_interaction_vars` must be set\nto NULL to perform multiple interaction")
    
    } 
    
    
    
    
  # 2. Run interaction for nominal variables ----
    
  if(!is.null(nominal_interaction_vars)){
    
    
    nom_vars <- nominal_interaction_vars ## Attribute a shorter name 
    subgroup_nominal <- list()
    
      
    for (nn in seq_along(nom_vars)) {
      
      
      # Check if subgroup variable is among the adjustment variables 
      
        if(any(adjustment_vars == nom_vars[nn]) ) {
          
          adjust_temp <- adjustment_vars[-which( adjustment_vars %in% nom_vars[nn] )] # Do not adjust by stratifying variables
          
          } else {adjust_temp <- adjustment_vars}
      
      
      
        if(any(stratify_vars == nom_vars[nn])) {
          
          stratify_temp   <- stratify[-which(stratify_vars %in% nom_vars[nn])]
          cox_strata_vars <- paste0("strata(", paste(stratify_temp, collapse = ", "), ")" )
          
          } else { cox_strata_vars <- paste0("strata(", paste(stratify_vars, collapse = ", "), ")" ) }
    


    # Create formula for stratified Cox analysis --
    
    model_vars <- paste(
      
      c(nom_vars[nn], 
        paste0(c(main_exposure_var, nom_vars[nn]), collapse = ":"),
        adjust_temp
        ),
      collapse = " + "
      
      )
    
    left_side     <- paste0("Surv(", fu.time, ", ", event_var, ")")
    # cox_strata_vars   <- paste0("strata(",  paste(cox_strata_vars, collapse = ", "),  ")" )
    
    form  <- as.formula(paste0(left_side, " ~ ", paste(model_vars, cox_strata_vars, sep = " + ")))
    
    
    # Fit cox model --
    model_int      <- survival::coxph(form, method = cox_method, data = dataset)
    model_int_summ <- summary(model_int)
    
    if(print_model_info == TRUE) {
      
      print(paste0("The Cox regression formula for ", nom_vars[nn], " is:\n \n")) 
      print(form)
      print(model_int_summ)
    
    }
     
    
    subgroup_nominal[[nn]]  <- 
      broom::tidy(model_int) %>% 
      dplyr::filter(grepl(main_exposure_var, term)) %>% 
      select(-statistic) %>% 
      mutate(
        term = stringr::str_replace_all(
                            term, 
                            pattern     = paste0(nom_vars[nn], "|:", main_exposure_var), 
                            replacement = ""
                            )
        )
    
    
    # Correct for regression dilution --
    
    if(correct_regression_dilution == TRUE){ 
      
      subgroup_nominal[[nn]] <- 
                subgroup_nominal[[nn]] %>% 
                      mutate(
                        est_corrected = estimate/regression_dilution_ratio, 
                        se_corrected  = std.error/regression_dilution_ratio, 
                        var_corrected = se_corrected^2, 
                        hr_corrected  = exp(est_corrected)
                      )
      
      }
    
    # Label subgroup variable --
    
    if(!is.null(nominal_labels)){
      
      if(is.null(nominal_interaction_vars)) stop("`nominal_interaction_vars` must be provided\n")
      if(length(nominal_labels) != length(nominal_interaction_vars)) stop("Length of `nominal_labels` and `nominal_interaction_vars` must be equal\n")
        
        
      subgroup_nominal[[nn]] <- 
               subgroup_nominal[[nn]] %>% 
               mutate(subgroup = nominal_labels[nn])
      
      }
    
    
    # Perform heterogeneity test and create a dataframe --
    
    het_list    <- heterogeneity(subgroup_nominal[[nn]]$est_corrected, subgroup_nominal[[nn]]$se_corrected)
    nrow_temp   <- nrow(subgroup_nominal[[nn]])
    
    het_df      <- data.frame(
      
      DF        = c(rep(NA, nrow_temp-1), het_list[[1]]), 
      Chi2      = c(rep(NA, nrow_temp-1), het_list[[2]]), 
      p_int     = c(rep(NA, nrow_temp-1), het_list[[3]]), 
      test_type = "heterogeneity")
    
    
    # Get number of events--
    events_temp <- dataset %>% 
                   dplyr::group_by(.data[[ nom_vars[nn] ]] ) %>% 
                   summarise(n_event = sum(.data[[event_var]] )) %>% select(n_event)
    
    # Bind trend test DF to main Subgroup DF --
    subgroup_nominal[[nn]] <- cbind(subgroup_nominal[[nn]], het_df, events_temp)
    
    
    # END OF FOR LOOP for Ordinal variables --
      
      }
    
    
    }
  
    

  # 3. Run interaction for ordinal variables --------
  
  if(!is.null(ordinal_interaction_vars)){
    
    
    ord_vars <- ordinal_interaction_vars ## Attribute a shorter name 
    subgroup_ordinal <- list()
    
    
    for (oo in seq_along(ord_vars)) {
      
      
      # Check if subgroup variable is among the adjustment variables 
      
      if(any(adjustment_vars == ord_vars[oo]) ) {
        
        adjust_temp <- adjustment_vars[-which( adjustment_vars %in% ord_vars[oo] )] 
        
        } else {adjust_temp <- adjustment_vars}
      
      
      
      if(any(stratify_vars == ord_vars[oo])) {
        
        stratify_temp   <- stratify[-which(stratify_vars %in% ord_vars[oo])]
        cox_strata_vars <- paste0("strata(", paste(stratify_temp, collapse = ", "), ")" )
        
        } else { cox_strata_vars <- paste0("strata(", paste(stratify_vars, collapse = ", "), ")" ) }
      
      
      
      # Create formula for stratified Cox analysis --
      
      model_vars <- paste(
        
        c(ord_vars[oo], 
          paste0(c(main_exposure_var, ord_vars[oo]), collapse = ":"),
          adjust_temp
        ),
        collapse = " + "
        
      )
      
      left_side     <- paste0("Surv(", fu.time, ", ", event_var, ")")
      # cox_strata_vars <- paste0("strata(",  paste(cox_strata_vars, collapse = ", "),  ")" )
      
      form  <- as.formula(paste0(left_side, " ~ ", paste(model_vars, cox_strata_vars, sep = " + ")))
      
      
      # Fit cox model --
      model_int      <- survival::coxph(form, method = cox_method, data = dataset)
      model_int_summ <- summary(model_int)
      
      if(print_model_info == TRUE) {
        
        print(paste0("The Cox regression formula for", ord_vars[oo], "is:\n")) 
        print(form)
        print(model_int_summ)
      
      }
      
      
      subgroup_ordinal[[oo]]  <- 
                broom::tidy(model_int) %>% 
                dplyr::filter(grepl(main_exposure_var, term)) %>% 
                select(-statistic) %>% 
                mutate(
                  term = stringr::str_replace_all(
                    term, 
                    pattern     = paste0(ord_vars[oo], "|:", main_exposure_var), 
                    replacement = ""
                  )
              )
      
      
      # Correct for regression dilution --
      
      if(correct_regression_dilution == TRUE){ 
        
        subgroup_ordinal[[oo]] <- 
                  subgroup_ordinal[[oo]] %>% 
                  mutate(
                    est_corrected = estimate/regression_dilution_ratio, 
                    se_corrected  = std.error/regression_dilution_ratio, 
                    var_corrected = se_corrected^2, 
                    hr_corrected  = exp(est_corrected))
        
      }
      
      # Label subgroup variable --
      
      if(!is.null(ordinal_labels)){
        
        if(is.null(ordinal_interaction_vars)) stop("`ordinal_interaction_vars` must be provided\n")
        if(length(ordinal_labels) != length(ordinal_interaction_vars)) stop("Length of `ordinal_labels` and `ordinal_interaction_vars` must be equal\n")
        
        
        subgroup_ordinal[[oo]] <- 
          subgroup_ordinal[[oo]] %>% 
          mutate(subgroup = ordinal_labels[oo])
        
      }
      
      
      # Perform heterogeneity test and create a dataframe --
      
      trend_list  <- trend(subgroup_ordinal[[oo]]$est_corrected, subgroup_ordinal[[oo]]$se_corrected)
      nrow_temp   <- nrow(subgroup_ordinal[[oo]])
      
      trend_df    <- data.frame(
        DF        = c(rep(NA, nrow_temp-1), trend_list[[1]]), 
        Chi2      = c(rep(NA, nrow_temp-1), trend_list[[2]]), 
        p_int     = c(rep(NA, nrow_temp-1), trend_list[[3]]), 
        test_type = "trend")
      
      
      # Get number of events--
      events_temp <- dataset %>% 
        dplyr::group_by(.data[[ ord_vars[oo] ]] ) %>% 
        summarise(n_event = sum(.data[[event_var]] )) %>% select(n_event)
      
      # Bind trend test DF to main Subgroup DF --
      subgroup_ordinal[[oo]] <- cbind(subgroup_ordinal[[oo]], trend_df, events_temp)
      
      
      # END OF FOR LOOP for ordinal variables --
    }
    
  }
    
   
    
    
  # 4. Run subgroup analysis, stratifying by more than one variable -----
       
       # For example age-sex interaction
       # For now, the function does not correct RDR of length > 1 (the mapping is prone to error)


  if(!is.null(multiple_interaction_vars) == TRUE) {
    
  
    # Do not adjust for subgroup vars 
    if(any(adjustment_vars %in% multiple_interaction_vars) ) {
      
      adjust_temp <- adjustment_vars[-which( adjustment_vars %in% multiple_interaction_vars )] 
      
      } else {adjust_temp <- adjustment_vars}
    
    
    
    if(any(stratify_vars %in% multiple_interaction_vars)) {
      
      stratify_temp <- stratify[-which(stratify_vars %in% multiple_interaction_vars)]
      cox_strata_vars   <- paste0("strata(", paste(stratify_temp, collapse = ", "), ")" )
      
      } else { cox_strata_vars <- paste0("strata(", paste(stratify_vars, collapse = ", "), ")" ) }
    
    
    # Create formula for stratified Cox analysis --
    model_vars <- paste(
                    c(multiple_interaction_vars,
                      paste0(c(main_exposure_var, multiple_interaction_vars), collapse = ":"),
                      adjustment_vars
                      ),
                    collapse = " + "
                    )

    left_side     <- paste0("Surv(", fu.time, ", ", event_var, ")")
    # cox_strata_vars   <- paste0("strata(",  paste(stratify_vars, collapse = ", "),  ")" )

    form  <- as.formula(paste0(left_side, " ~ ", paste(model_vars, cox_strata_vars, sep = " + ")))


    # Fit cox model --
    model_int      <- survival::coxph(form, method = cox_method, data = dataset)
    model_int_summ <- summary(model_int)

    if(print_model_info == TRUE) {
      
       print("The Cox regression formula is:\n") 
       print(form)
       print(model_int_summ)
       
    }


    model_df  <-
        broom::tidy(model_int) %>%
        dplyr::filter(grepl(pattern = main_exposure_var, term)) %>%
               arrange(term) %>%
               select(-statistic) %>%
        dplyr::mutate(

                term = str_replace_all(
                              term,
                              pattern     = paste0( paste0(multiple_interaction_vars, "|", collapse = ""), ":", main_exposure_var ),
                              replacement = ""
                              )

                ) %>%
        tidyr::separate(col = term, into = multiple_interaction_vars, sep = ":")


    # Correct for regression dilution --
    if(correct_regression_dilution == TRUE){

      model_df <-
            model_df %>%
            mutate(
              est_corrected = estimate/regression_dilution_ratio,
              se_corrected  = std.error/regression_dilution_ratio,
              var_corrcted  = se_corrected^2)
    
      }
    
    
    
  }
    

    # Return value --
    
    if(!is.null(nominal_interaction_vars) && !is.null(ordinal_interaction_vars)) return( c(subgroup_nominal, subgroup_ordinal) )
    else if (!is.null(nominal_interaction_vars) ) return(subgroup_nominal)
    else if (!is.null(ordinal_interaction_vars) ) return(subgroup_ordinal)
    else return(model_df)
    
  }




  #   
  #   ## DO NOT correct for regression dilution 
  #   
  #   if(expo_type_continuous == TRUE  && 
  #      correct_regression_dilution == FALSE) {
  #     
  #     # Make formula
  #     model_vars   <- paste(c(exposure_var_cont,adjustment_vars), collapse = " + ")
  #     form         <- as.formula(paste0(left_side, " ~ ", paste(model_vars, cox_strata_vars, sep = " + ")))
  #     
  #     model_cont   <- survival::coxph(form, method = cox_method, data = dataset)
  #     model_cont_summ <- summary(model_cont)
  #     print(model_cont_summ)
  #     
  #     model_df  <- 
  #       broom::tidy(model_cont) %>% 
  #       dplyr::filter(grepl(pattern = exposure_var_cont, term)) %>% 
  #       dplyr::mutate(hr  = exp(estimate), 
  #                     lci = exp(estimate - 1.96*std.error), 
  #                     uci = exp(estimate + 1.96*std.error)
  #       ) %>% 
  #       dplyr::select(-statistic)
  #     
  #     
  #     return(list("model_object" = model_cont, "dataframe" = model_df))
  #     
  #   }    
  #   
  #   
  #   
  # }


