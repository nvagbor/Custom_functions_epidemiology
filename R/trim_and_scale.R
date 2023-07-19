# The problem
# To calculate the standard deviations of points of in a vector subset. 
## For example: 
## Calculating the SDs for points for BMI > 22 kg/m2

trim_and_scale <-
  function(dataset,
           x,
           threshold_expr,
           get_trimmed_var = FALSE) {
    
    if (missing(dataset)) {
      # Allow user to provide a vector
      
      trimmed_var <- eval(parse(text = paste0(
        "ifelse(x", threshold_expr, ", x, NA)"
      )))
      
    } else{
      df <- dataset
      
      trimmed_var <- eval(parse(
        text = paste0("ifelse(df$", x, threshold_expr, ",", "df$", x, ", NA)")
      ))
      
    }
    
    scaled_var <- as.numeric(scale(trimmed_var))
    
    
    #Return value --
    
    if (get_trimmed_var == FALSE) {
      return(scaled_var)
      
    } else if (get_trimmed_var == TRUE) {
      return(list(scaled_variable = scaled_var,
                  trimmed_variable = trimmed_var))
      
    }
    
    
  }


