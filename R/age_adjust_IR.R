####### Age-adjusted incidence rates using poisson regression for overall dataset(Edited)
#
# Dependencies : dplyr, stdReg
#   
# Per: Numeric of length 1. 

library(dplyr)
library(stdReg)
library(survival)

age_sex_adjust <- function(data_var,  
                           age_var,
                           event_var, 
                           time_var){
  
  age_var   <- deparse(substitute(age_var))
  event_var <- deparse(substitute(event_var))
  time_var  <- deparse(substitute(time_var))
  

  offset       <- log(as.numeric(data_var[[time_var]]/365.25/100))
  data_trans   <- transform(data_var, time_var = 36525) # To get IR since offset is specified as days/362.25/100
  formula      <- formula(paste0(event_var, 
                                 " ~ ",  
                                 age_var))
  
  poisson_fit <- glm(formula = formula, 
                     offset  = offset,
                     data    = data_var,
                     family  = poisson)
  
  
  std_fit <- stdGlm(poisson_fit, data = data_trans, X = age_var)
  
  std_sum <- summary(std_fit, CI.type = "log") # To correct CI use type = "log", otherwise possible with negative values
  
  std_sum[["est.table"]] %>% 
    as_tibble() %>% 
    transmute(IR = Estimate, 
              lower = `lower 0.95`, 
              upper = `upper 0.95`)
}