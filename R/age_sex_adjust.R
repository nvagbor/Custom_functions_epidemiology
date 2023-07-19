####### Age- and sex-adjusted incidence rates using poisson regression
#
# Dependencies : dplyr, stdReg
#   
# Vignette here: https://www.r-bloggers.com/2021/11/age-and-sex-adjusted-incidence-rates/


library(dplyr)
library(stdReg)
library(survival)

age_sex_adjust <- function(data_var, 
                            group_var, 
                            age_var,
                            sex_var,
                            event_var, 
                            time_var){
  
  group_var <- deparse(substitute(group_var))
  age_var   <- deparse(substitute(age_var))
  sex_var   <- deparse(substitute(sex_var))
  event_var <- deparse(substitute(event_var))
  time_var  <- deparse(substitute(time_var))
  
  group_levels <- levels({{ data_var }}[[{{ group_var }}]])
  offset       <- log(as.numeric(data_var[[time_var]]/365.25/100))
  data_trans   <- transform(data_var, time_var = 36525) # To get IR since offset is specified as days/362.25/100
  formula      <- formula(paste0(event_var, 
                                 " ~ ", 
                                 group_var, 
                                 " * ",
                                 "I(", 
                                 age_var, 
                                 "^2)", 
                                 " + ", 
                                 sex_var))
  
  poisson_fit <- glm(formula = formula, 
                     offset  = offset,
                     data    = data_var,
                     family  = poisson)
  
  
  std_fit <- stdGlm(poisson_fit, data = data_trans, X = group_var)
  
  std_sum <- summary(std_fit, CI.type = "log") # To correct CI use type = "log", otherwise possible with negative values
  
  std_sum[["est.table"]] %>% 
    as_tibble(rownames = group_var) %>% 
    transmute({{ group_var }} := group_levels,
              IR = Estimate, 
              lower = `lower 0.95`, 
              upper = `upper 0.95`)
}