## Function to reorder a column by order precised by the user 


order_row <- function(data, 
                      new_col_order,
                      old_col_name
                      ) {
  
  # Old and new unique values 
  old_values  <- unique(data[[ old_col_name ]])
  new_values  <- unique(new_col_order)
  
  # Old and new unique vector length 
  len_new_col <- length(new_values)
  len_old_col <- length(old_values)
  
  
  if(len_new_col != len_new_col) stop("Length of unique values for old and new vectors must be the same")
  
  if(!all( old_values %in% new_values)) stop("All values of new vector must be present in old vector")
  
  # Get index to order DF by new order
  i_order <- seq_len(length(new_values))
  
  
  # Create an index variable to order rows
  data$i_nrow <- NA
  
      
      for (i in seq_along(new_values)) {
        
        data$i_nrow  <- ifelse(data[[old_col_name]] == new_values[i], i_order[i], data$i_nrow) 
        
      }
      
  # arrange by i_row
  data <- data[order(data$i_nrow), ]
  data$i_nrow <- NULL
        
        
  return(data)    
  
  
  
}


