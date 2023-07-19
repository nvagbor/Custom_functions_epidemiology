
# Function prepares data for a linear plot 

# Aims
## 1. Calculate mean values of some variables (outcome_vars) by groups of another variable (x_var_cat)
## 2. x_var_cat is a continuous variable (x_var_cont) that has been categorised 
## 3. The mean values of the outcome_var and x_var_conts by groups of x_var_cat will be calculated and plotted
## 4. The means of outcome_var should be standardised



Make_LineData_Mean <- function(
    
  dataset, 
  group_var,                # Separate plots will be produced for this group (e.g., me and women)
  outcome_var, 
  outcome_var_lab, 
  x_var_cat,                    # Variable to be plotted on the x axis (x_var_cat is the categorical form of x_var_cont)
  x_var_cont,               # Continuous form of x_var_cat
  adjustment_variables = NULL, 
  n_decimal = 1
  
){ 
  
  group_var_levels <- levels(as.factor(dataset[[group_var]]))
  
  
  if(!is.null(outcome_var_lab)) {

    if (length(outcome_var_lab) != length(outcome_var)) {

      stop("Length of `outcome_var` and `outcome_var_lab` must be the same")

    } else{

      outcome_var_lab <- outcome_var

    }

  }
  
  
    df_mean <- list()
    
    for (k in group_var_levels){
      
      
      df_mean[[k]] <-
        
        purrr::map2_dfr( #Rowbind and prepare for plotting --
          
          .x = outcome_var,
          .y = seq_along(outcome_var),
          ~DirectStandardisation::adjmeans(
            data              = dataset %>% filter(.data[[group_var]] == k) %>% as.data.frame(.), # Coerce into a data.frame
            outcome_vars      = .x, 
            categorical_vars  = x_var_cat, 
            adjustment_vars   = adjustment_variables) %>%  
            
            `row.names<-`(NULL) %>%
            
            dplyr::rename( 
              "var_name"     = dplyr::contains("variable"),
              "strata"       = dplyr::contains("levels"), 
              "N"            = dplyr::contains("Freq"),
              "mean_var"     = dplyr::contains(".mean"),
              "variance_var" = dplyr::contains("variance"),
              "se_var"       = dplyr::contains(".se")) %>%
  
            dplyr::transmute(
  
              var_name     = var_name,
              strata       = strata,
  
              strata_mean  = dataset %>%
                               dplyr::filter(.data[[group_var]] == k) %>%
                               dplyr::group_by(.data[[x_var_cat]]) %>%
                               dplyr::summarise(
                                        mean_expo = mean(.data[[x_var_cont]], na.rm = TRUE),
                                       .groups   = "drop") %>%
                               dplyr::pull(mean_expo),
  
              sd_var       = format(round(se_var * sqrt(N), n_decimal), nsmall = n_decimal),
              mean_sd      = paste0(format(mean_var, nsmall = n_decimal), " (", sd_var, ")"),
              mean_var     = mean_var,
              se_var       = se_var,
              LCI_var      = mean_var - 1.96*se_var,
              UCI_var      = mean_var + 1.96*se_var,
              outcome_var  = outcome_var_lab[.y],
              group_level  = k
              
              )
  
        )
      
    # End of For Loop
      }

  # Convert outcome var into a factor --
  outcome_var_levels <- unique(df_mean[[1]]$outcome_var)

  df_mean <-
    purrr::map(
      .x = df_mean,
      ~dplyr::mutate(
        .x,
        outcome_var = factor(outcome_var, levels = outcome_var_levels))
    )

    
  
  return(df_mean)
  
}
