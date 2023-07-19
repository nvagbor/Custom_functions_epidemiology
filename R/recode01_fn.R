# Write function to recode and label 0 and 1 to Yes/No

# Dependencies: tidyverse
my_recode01_fn <- function(
     dataset, 
     col_name_old, 
     col_name_new = NULL,
     label_for_1  = "Yes", 
     label_for_0  = "No",
     to_factor    = FALSE) {
  
  # col_old_sym <- rlang::sym(col_name_old)
  
 
  # Use name column name as label if one is provided
  if(is.null(col_name_new)){
    col_name <- rlang::sym(col_name_old)
  } 
  else{
    col_name <- rlang::sym(col_name_new) 
    }
   
      
  if (to_factor == TRUE) {
  
    dataset<- 
      dataset %>% 
      dplyr::mutate (
        "{{col_name}}" := factor(
          dplyr::case_when(
              .data[[col_name_old]] == 1 ~ label_for_1, 
              .data[[col_name_old]] == 0 ~ label_for_0, 
              TRUE ~ NA_character_),
          levels = c(label_for_1, label_for_0)
          )
        )
                 
  } else{
    
    dataset<- 
      dataset %>% 
      dplyr::mutate (
        "{{col_name}}" := dplyr::case_when(
            .data[[col_name_old]] == 1 ~ label_for_1, 
            .data[[col_name_old]] == 0 ~ label_for_0, TRUE ~ NA_character_)
        
        )
    
    
  }

 
        
  return(dataset)
        
}


