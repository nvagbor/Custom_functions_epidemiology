## Function takes a dataframe containing results by categories of a group variable
## Without variable label 
## Adds the variable label above the variable categories
## The data must containing a column of variable to be used as group heading and 
## a column containing the names of group levels 

## Extra_space argument adds an extra space with of missing value above the inserted headers

## This is important for producing a table for reporting 
## Or formating data for producing forest plot using Jasper 


## Dependencies: Tidyverse, purrr




add_table_header <- function(data, 
                              group_var,
                              group_level_var, 
                              extra_space = FALSE) {
  
  # Get unique levels of group heading 
  group_headers <- data %>% pull({{group_var}}) %>% unique(.)
  
  
  # Add spaces above group names and insert group headings 
  
 if(extra_space == TRUE) {
   
   tab_df <- data %>% 
     
     # Stabilise order of group headings before grouping 
     dplyr::mutate("{{group_var}}" := factor({{group_var}}, levels = group_headers)) %>% 
     dplyr::group_by({{group_var}}) %>% dplyr::group_split() %>% 
     purrr::map2(.y = {{group_headers}},  
                 ~ .x %>% 
                    add_row(.before = 1, "{{group_level_var}}" := .y) %>% 
                    add_row(.before = 1) ) %>% # Not sure why this does not work
     
     # Bind dataframe 
     map_dfr( ~rbind(.x) %>% select(-{{group_var}}))
   
   return(tab_df)
 
  }
  
  
  tab_df <- data %>% 
    
    # Stabilise order of group headings before grouping 
    dplyr::mutate("{{group_var}}" := factor({{group_var}}, levels = group_headers)) %>% 
    dplyr::group_by({{group_var}}) %>% dplyr::group_split() %>% 
    
    purrr::map2(.y = {{group_headers}},  
                ~ .x %>% 
                  dplyr::add_row(.before = 1, "{{group_level_var}}" := c(.y))) %>% 
    
    # Bind dataframe 
    map_dfr( ~rbind(.x) %>% select(-{{group_var}}))
  
  
  return(tab_df)
  
}


