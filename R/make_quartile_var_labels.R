
# Create quartile of numeric variables 
# Multiple variables can be passed 
# Programmatical determine the ranges of each category 
# Use to label the categories based on some predefine prefix, e.g. Q(1-10)
# Can add suffix for the new variable name
# Dependencies: dplyr and quantcut 


make_quartile_var_labels <- function(dataset, 
                                     numeric_vars, 
                                     n_quartile, 
                                     prefix = "Q", 
                                     var_name_suffix = "_Q", 
                                     n_decimal = 0){
  
  
  # Make quartiles ---
  
  q_labels <- paste0(prefix, seq_len(n_quartile))
  suffix   <- paste0(var_name_suffix, n_quartile)
  
  dataset <- 
    dataset %>% 
    dplyr::mutate(
      across(
        
        .cols = all_of(numeric_vars),
        ~factor(gtools::quantcut(.x, n_quartile), labels = q_labels),
        .names = paste0("{.col}", suffix)
      
        )
    )
  
  
  
  # Generate labels ---
  # Only use labels for HF
  
  num_var_q  <- paste0(numeric_vars, suffix) # Get names of newly created quartile variables  
  
  for (q in seq_along(num_var_q)) {
    
    # Generate quartile labels 
    
    quantile_labels <-  
      dataset %>% 
      dplyr::group_by( .data[[ num_var_q[[q]] ]] ) %>% 
      
      dplyr::summarise(
        var_min = min( .data[[ numeric_vars[[q]] ]], na.rm = TRUE), 
        var_max = max( .data[[ numeric_vars[[q]] ]], na.rm = TRUE)
      ) %>% 
      
      dplyr::mutate(
        q_level = .data[[ num_var_q[[q]] ]],
        
        q_labels = dplyr::case_when(
          
            q_level == q_labels[[1]] ~ paste0(q_level, " (<", format(var_max, nsmall = n_decimal), ")"),
            q_level == q_labels[[n_quartile]] ~ paste0(q_level, " (", format(var_min, nsmall = n_decimal), "+)"),
            TRUE ~ paste0(
              q_level, " (",
              format(var_min, nsmall = n_decimal), 
              " to <", 
              format(var_max, nsmall = n_decimal), ")"
              ) 
        
            )
      
        ) %>% 
      dplyr::pull(q_labels)
    
    
    # Labels quartile variables ---
    dataset[ , num_var_q[[q]] ] <- factor(dataset[[num_var_q[[q]]]], labels = quantile_labels)
    
  }
  
  
  return(dataset)
  
  
}

