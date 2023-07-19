# Object: Function 
# Objective: Generate a vector of inverse probability weights that can be used for adjustment of survival curves of two groups

# What does it do? 
    # Generate probabilities of a stratifying variable (e.g.diabetes or not) 
    # based on groups in the data such as age, sex, and area

# Arguments 
    # data: The dataframe containing containing variables of interest 
    # grouping_var: Character vector of name(s) of variables to be used for adjustment (e.g., age, sex, area (urban/rural))
    # strata: character vector oflength one containing the name of stratifying variable. 
    # weight_col: The default weight column will be labelled "weight" if no name is supplied. 
    # print_weight_col_name: Print the name of the weight column in the dataset 

# Value
    # Returns a tibble with a containing column containing the IP weight. 
    # Returns the name of weight variable


# Note: The function is a wrapper for the tidyverse function - the user need to install and load tidyverse

# Example: my_ipw_fn (data = df, 
#                     grouping_var = c("age_group", "sex", "area"), 
#                     strata = "education", 
#                     weight_col = "my_weight_col")


my_ipw_fn <- function(data, 
                      grouping_var, 
                      strata, 
                      weight_col, 
                      print_weight_col_name = FALSE) {
  
  sample_size <- nrow(data)
  
  #Probabilities of groups  
  grp_props    <-  
      data %>% 
      select(all_of(c(grouping_var, strata))) %>% 
      dplyr::group_by(!!!rlang::syms(grouping_var), .drop = FALSE) %>% 
      dplyr::summarise(prop_grp = n()/sample_size, .groups = "keep") %>% 
      ungroup()
  
  
  #Probabilities of strata giving groups 
  strata_props <-  
      data %>% 
      select(all_of(c(grouping_var, strata))) %>% 
      dplyr::group_by(!!!rlang::syms(c(grouping_var, strata)), .drop = FALSE) %>% 
      dplyr::summarise(prop_strata = n()/sample_size, .groups = "keep") %>% 
      ungroup()
  
  
  #Merge data and calculate weights 
  weight_df   <- 
      strata_props %>% 
      dplyr::group_by(!!rlang::sym(strata), .drop = FALSE) %>% 
      dplyr::inner_join(grp_props, by=grouping_var) %>% 
             ungroup() %>% 
                 
      dplyr::mutate(wt = prop_strata / prop_grp) %>%  
             select(-c(prop_grp, prop_strata)) %>% as.data.frame()
                          
 
  # Thrwo warning if there are infinite weigts in the dataset  
  if(is.infinite(max(weight_df$wt, na.rm = TRUE))) {warning("Presence of infinite weights")}
  
  #Rename weights to have one weight column for each variable 
  if(missing(weight_col)) {
    
        weight_lab <- "weight"
        
        if(print_weight_col_name == TRUE) {cat("The name of the weight column is: ", weight_lab)}
        
        names(weight_df)[names(weight_df) == "wt"] <- weight_lab
  
        }
  
  else{
    
        weight_lab <- weight_col
        
        if(print_weight_col_name == TRUE) {cat("The name of the weight column is: ", weight_lab)}
        
        names(weight_df)[names(weight_df) == "wt"] <- weight_lab
  }
  
  # #Combine weight to dataset
  
  data <- data %>% dplyr::inner_join(weight_df, by=c(grouping_var, strata))
  
  return(data)
  
  
}
