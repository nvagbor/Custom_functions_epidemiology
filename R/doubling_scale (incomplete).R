


doubling_scale <- function(min_val, max_val, n_ticks) {
  
  
  if(!missing(n_ticks)) {nice_labs <- pretty(min_val:max_val, n = n_ticks)}
  
  else {nice_labs <- pretty(min_val:max_val)}
    

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

  
  if(min_val == 0) {
    
    # Add 2 to avoid infinite numbers
    n_power  <- (max_val/(min_val+2))/2
    
  }
  
  else {
    
    n_power  <- (max_val/(min_val))/2
    
  }
  
  tick.seq <- c(0:n_power)
  
  tick_list <- list(length = length(tick.seq))
  
  
  for (i in seq_along(tick.seq)) {
    
                tick.val <- (min_val + 2)*2^tick.seq[[i]]
              
        if (tick.val <= max_val) {
                        tick_list[[i]] <- roundUpNice(tick.val)
        }
        
        else {
          tick_list[[i]] <- NULL
        }
                
    # End of For Loop
  }
    
    if(min_val == 0){
      
      tick_vect <- c(0, unlist(tick_list))
    }
      
    else {
      
      tick_vect <- c(unlist(tick_list))
    }
    
    return(tick_vect)
}





