
# Function to create data for standardised means and proportions for CKB data
# Data can be edited with other packages like Flextable to make publication-ready tables 
# Package dependencies: Tidyverse, directStandardisation package

# The main exposure must be a categorical variable

make_standardised_ckb_data <- function(dataset, 
                                       main_exposure,
                                       numeric_vars = NULL, 
                                       numeric_vars_labels = NULL, 
                                       categorical_vars = NULL, 
                                       categorical_vars_labels = NULL, ## Preferable to not have this in
                                       adjustment_variables, 
                                       sex_specific_analysis = FALSE,
                                       sex_var = NULL,
                                       n_decimals = 1
                                       ) {


  # Checks ---
  
    if(missing(main_exposure)) stop("please enter `main exposure`")
      
    if(is.numeric(dataset[[main_exposure]]) | is.integer(dataset[[main_exposure]])) {
      stop("The `main exposure` cannot be a numeric or integer vector")  
      }
  
  
    # Verify length if variable names and labels are provided 
    if(!is.null(numeric_vars) & !is.null(numeric_vars_labels)){
      
      stopifnot(length(numeric_vars) == length(numeric_vars_labels))
      
    }
  
    if(!is.null(categorical_vars) & !is.null(categorical_vars_labels)){
      
      stopifnot(length(categorical_vars) == length(categorical_vars_labels)) 
      
    }
    
  # Force dataset into a dataframe 
    
  dataset <- dataset %>% as.data.frame(.)
    
    
  # 1. For NUMERIC variables ----------------------------------------------------------------

if(!is.null(numeric_vars)){
    
    dataset_temp <- dataset %>% 
                    dplyr::select(all_of(c(numeric_vars, main_exposure, adjustment_variables)))
  
  
    numeric_vars_df <- list()
    
    for (nv in seq_along(numeric_vars)) {
      
      num_var_temp <- numeric_vars[[nv]]
      
      
      numeric_vars_df[[nv]] <- 
        
        DirectStandardisation::adjmeans(
          dataset           = dataset_temp, 
          outcome_vars      = num_var_temp,
          categorical_vars  = main_exposure, 
          adjustment_vars   = adjustment_variables, 
          ndigits           = 4
          
        ) %>% 
        
        # Wrangling 
        transmute(
          
          cat_var_lab = !!sym(paste0(num_var_temp, ".levels")),
          stdev       = !!sym(paste0(num_var_temp, ".se")) * sqrt( !!sym(paste0(num_var_temp, ".N.Freq")) ), 
          mean_sd     = paste0(round( !!sym(paste0(num_var_temp, ".mean")) , n_decimals), " (", 
                               round(stdev, n_decimals), ")"
          )
          
        )  %>% 
        
        select(-stdev) %>% 
        
        pivot_wider(names_from = cat_var_lab, values_from = mean_sd) %>% 
        
        mutate(exposure = num_var_temp, 
               all      = paste0(round(mean(dataset_temp [ , num_var_temp], na.rm = TRUE), n_decimals), " (", 
                                 round(sd  (dataset_temp [ , num_var_temp], na.rm = TRUE), n_decimals), ")")
        ) %>% 
        
        relocate(.before = 1, exposure)
      
      
      # Assign labels 
      if(!is.null(numeric_vars_labels)) {numeric_vars_df[[nv]]$exposure <- numeric_vars_labels[[nv]]}
      
      
      # End of For Loop
    }
    
  # Bind list of dataframes 
  df_numeric_var <- dplyr::bind_rows(numeric_vars_df)
  
}
  
  
  ## 2. For CATEGORICAL VARIABLES ---

  if(sex_specific_analysis == TRUE){
    
    dataset_temp <- 
      dataset %>% 
      select(all_of(c(sex_var, categorical_vars, main_exposure, adjustment_variables)))
    
  } else{
    
    dataset_temp <- 
      dataset %>% 
      select(all_of(c(categorical_vars, main_exposure, adjustment_variables))) 
    
  }
  
  
if(!is.null(categorical_vars)){
  
  
  cat_var_list <- list()
  
  for (c in seq_along(categorical_vars)) {
    
    cat_var_temp <- categorical_vars[[c]]
    
    # Get adjusted proportions ---
    
    cat_var_list[[c]] <- 
      
      DirectStandardisation::adjprop(
        
        dataset                = dataset_temp,
        outcome_vars           = cat_var_temp,
        categorical_vars       = main_exposure,
        adjustment_vars        = adjustment_variables
        
      ) %>% 
      
      dplyr::mutate_at(vars(contains("proportion")), ~round(.*100, n_decimals)) %>% 
      
      select(contains(c("levels", "Freq", "proportion"))) %>% 
      `row.names<-` (paste0("Q", 1:length(unique(dataset_temp[[main_exposure]])) )) %>%  
      t(.) %>% 
      
      `row.names<-` (
        
        str_replace_all(string      = row.names(.), 
                        pattern     = paste0(cat_var_temp, "."), 
                        replacement = ""
                       ) %>% 
                      
        str_replace_all(pattern = "proportion.", replacement = "")
        
        ) %>% 
      
      as.data.frame(.) 
    
          # If sex-specific analysis, define exposure differently
      if(sex_specific_analysis) {
        
        cat_var_list[[c]] <- 
          
          cat_var_list[[c]] %>% 
          dplyr::mutate(exposure = paste0(unique(dataset_temp[[sex_var]]), "_", cat_var_temp, "_", row.names(.)))%>% 
          dplyr::relocate("exposure", .before = "Q1") %>% 
          `row.names<-`(NULL) %>% 
          dplyr::slice(-c(1:2))
        
      } else{
    
      cat_var_list[[c]] <- 
        cat_var_list[[c]] %>% 
        dplyr::mutate(exposure = paste0(cat_var_temp, "_", row.names(.))) %>% 
        dplyr::relocate("exposure", .before = "Q1") %>% 
        `row.names<-`(NULL) %>% 
        dplyr::slice(-c(1:2))
      }
    
    # Get overall proportions 
    
    prop_overall <- 
      
      dataset_temp %>% 
          dplyr::group_by( .data[[cat_var_temp]] ) %>% 
          dplyr::summarise(
            
            n = n(), 
            N = length( dataset_temp[[cat_var_temp]] ), 
            prop100 = round((n/N)*100, n_decimals) 
            
          ) %>% 
          dplyr::select(all_of(cat_var_temp), prop100)
     
    
    # Bind overall proportion df to main dataframe 
    cat_var_list[[c]]$all <-  prop_overall$prop100
    
    
    # End of For Loop
  }
  
  # Bind list of dataframes 
  df_cat_var <- dplyr::bind_rows(cat_var_list)
  
}  
 
  
  # 3. Set conditions to return dataframe ---
    
    # Return both dataframes 
    if(!is.null(numeric_vars) & !is.null(categorical_vars)){
      
      df_stdz <- rbind(df_numeric_var, df_cat_var) 
      return(df_stdz)
      
    }
    
    # Return DF of numeric variables 
    if(!is.null(numeric_vars) & is.null(categorical_vars)){
      
      df_stdz <- df_numeric_var
      return(df_stdz)
      
    }
  
    # Return DF of categorical variables 
    if(is.null(numeric_vars) & !is.null(categorical_vars)){
      
      df_stdz <- df_cat_var
      return(df_stdz)
      
    }
  
  
# End of Function
}

# Returns a tibble