# Standardised incidence rate function by Neil Wright 

# Personal modifications
#=======================

# 1. Changed name for age-at-risk variable to age_lexis to avoid conflicts if there
# is a dataset called "age" in the dataset

# 2. Separate the functions to expand time axis as this causes the function to slow down 
# If one wants to use a loop 
# The function will then need to take an expanded dataset as dataframe 



# Lexis expansion
lexis_expansion <- function(data, 
                            studyid       = "studyid",
                            birth_date    = "birth_date",
                            entry_date    = "study_date",
                            exit_date     = "endpoint_date",
                            status        = "endpoint",
                            agegrp_breaks = seq(30, 100, 10)) {
  
  # using Epi package
  # Dates are converted to a numerical value, giving the calendar year as a fractional number,
  # assuming that years are all 365.25 days long, so inaccuracies may arise
  # see ?cal.yr for more details
  
  data_in_years <- 
    
    data %>% 
      mutate(
        status     = !!rlang::sym(status),
        studyid    = !!rlang::sym(studyid),
        age_entry  = cal.yr(!!rlang::sym(entry_date)) - cal.yr(!!rlang::sym(birth_date)),
        age_exit   = cal.yr(!!rlang::sym(exit_date))  - cal.yr(!!rlang::sym(birth_date)),
        entry_date = cal.yr(!!rlang::sym(entry_date)),
        exit_date  = cal.yr(!!rlang::sym(exit_date))
        )
  
  lexis_data <- Lexis(
    entry        = list("calendar" = entry_date, "age_lexis" = age_entry),   
    exit         = list("calendar" = exit_date),
    entry.status = 0,
    exit.status  = status,
    data         = data_in_years,
    id           = studyid, 
    tol          = 0)
  
  expanded <- splitLexis(lexis_data, 
                         breaks     = agegrp_breaks, 
                         time.scale = "age_lexis", 
                         tol        = 0) %>%
              mutate(agegrp = cut(age_lexis, breaks = agegrp_breaks, right = FALSE)) %>%
              filter(!is.na(agegrp))
  
  return(expanded)
  
}



incidence_rates <- function(data,
                            group     = "is_female",
                            strata    = "region_code",
                            per_pyar  = 1000){
  
  
  # calculate PYAR, no. of events, and incidence rate in groups formed by
  # group column and all strata columns
  ir_in_all_strata <- data %>%
                      group_by(!!!rlang::syms(c(group, strata))) %>%
                      summarise(pyar = sum(lex.dur), nevents = sum(lex.Xst)) %>%
                      ungroup() %>% 
                      mutate(incidencerate = nevents / pyar)
  
  
  # calculate PYAR in groups formed by strata and agegrp columns (standard population)
  pyar_across_strata <- ir_in_all_strata %>%
                        group_by(!!!rlang::syms(strata)) %>%
                        summarise(std_pyar = sum(pyar))
  
  total_std_pyar     <- sum(pyar_across_strata$std_pyar)
  
  
  # calculate standardised incidence rates in groups formed by group column
  ir_in_all_strata   <- inner_join(ir_in_all_strata, pyar_across_strata, by = strata) %>% 
                        mutate(
                          std_nevents = incidencerate * std_pyar,
                          var  = (std_pyar / total_std_pyar)^2 * (incidencerate)^2 / nevents 
                          )
  
  std_ir <- ir_in_all_strata %>%
            group_by(!!!rlang::syms(group)) %>%
            summarise(
              std_nevents   = sum(std_nevents),
              std_pyar      = sum(std_pyar),
              var           = sum(var),
              crude_nevents = sum(nevents),
              crude_pyar    = sum(pyar)) %>%
            
            ungroup () %>% 
            mutate(
              std_ir    = std_nevents / (std_pyar / per_pyar),
              std_ir_se = sqrt(var) * per_pyar,
              crude_ir  = crude_nevents / (crude_pyar / per_pyar)) %>% 
    
            select(group, crude_nevents, crude_pyar, crude_ir, std_ir, std_ir_se)
  
  if (min(ir_in_all_strata$nevents) < 20) { 
    
    warning("There are strata groups with fewer than 20 events.\n") 
    
    }
  
  return(list(ir_in_all_strata, std_ir))
}

