# Transpose a dataframe using dplyr 

df_transpose <- function(df) {
  
  first_name <- colnames(df)[1]
  
  temp <-
    df %>% 
    tidyr::pivot_longer(-1) %>%
    tidyr::pivot_wider(names_from = 1, values_from = value)
  
  colnames(temp)[1] <- first_name
  
  return(temp)
}