
# Function to collapse a vector of strings with comma, 
# Then add "and" before the last string 
# Can  be useful for reporting 
# For example: c("apples", "banana", "mango", "orange") becomes 
### "apples, banana, mango, and orange"


string_add_and <- function(x, add_period = FALSE){
  
    if(add_period == FALSE){
      str_fomatted <- 
        paste0(
            paste0(x[seq_len(length(x) - 1)], collapse = ", "), 
            ", and ", 
            x[length(x)]
        )
      
    } else {
      
      str_fomatted <-  
        paste0(
            paste0(x[seq_len(length(x) - 1)], collapse = ", "), 
            ", and ", 
            x[length(x)], "."
        
      )
    
    }
    
    return(str_fomatted)

}
