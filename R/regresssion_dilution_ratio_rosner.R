# Function to calculate regression dilution ratio using Rosner regression method 
# Takes datasets containing baseline and resurvey measurements

rdr_rosner <- function(
    dataset, 
    baseline_var, 
    resurvey_var, 
    adjustment_variables = NULL){
  
  
  # Make regression formula -- 
  if(!is.null(adjustment_variables)){
    
    covariates <- paste(
      resurvey_var, 
      paste(adjustment_variables, collapse = " + "), sep = " + ")
    
  }else{covariates <- resurvey_var}
  
  
  regress_formula <- as.formula( paste(baseline_var, "~", covariates) )
  
  
  # Fit linear regression --
  Regression_model <- lm(formula = regress_formula, data = dataset) 
  
  df_model <- 
    Regression_model %>% 
    broom::tidy(.) %>% 
    dplyr::filter(term == resurvey_var) %>% 
    dplyr::mutate(
      lci = estimate - 1.96*std.error, 
      uci = estimate + 1.96*std.error, 
      est_ci = paste0(
        format(round(estimate, 2), nsmall = 2), " (", 
        format(round(lci, 2), nsmall = 2), " - ", 
        format(round(uci, 2), nsmall = 2), ")"
      )
    )
  
  
  # Create output -- 
  RDR <- list(
    "fit"      = regress_formula,
    "model"    = Regression_model, 
    "df_model" = df_model, 
    "rdr"      = df_model$estimate)
  
  return(RDR)
  
  
}