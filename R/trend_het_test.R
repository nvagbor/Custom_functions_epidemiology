trend <- function(beta, se) {
  
  # beta must be a vector of estimates
  # se must be a vector of their standard errors
  
  if (length(beta) != length(se)) {
    stop("Length of vector of estimates must be equal to length of vector of standard errors.")
  }
    z    <- seq(1:length(beta))
    w    <- 1 / (se^2)
    
    test <- (sum(w * beta *(z - (sum(z * w) / sum(w))))^2) / sum(w * ((z - sum(z * w) / sum(w))^2))
    
    p    <- pchisq(test, df = 1, lower = FALSE)
    
    return(list("df" = 1, "test statistic" = test, "p" = p))
}


heterogeneity <- function(beta, se) {
  
  # beta must be a vector of estimates
  # se must be a vector of their standard errors
  
  # degrees of freedom	
  df <- length(beta) - 1
  
  # expected_beta: inverse variance weighted average of betas
  expected_beta <- sum(beta / se^2) / sum(1 / se^2)
  
  heterogeneity_test_statistic <- sum(((beta - expected_beta) / se)^2)
  
  p <- pchisq(heterogeneity_test_statistic, df = df, lower = FALSE)
  
  return(list("df" = df, "test statistic" = heterogeneity_test_statistic, "p" = p))
  
}
