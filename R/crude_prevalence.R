
# Function to calculate crude prevalence simultaneously for multiple groups
# Returns the prevalence and 95CI

# Dependencies: stringr, purrr


crude_prevalence <- function(data, 
                             outcome, 
                             outcome.label,
                             exposures) {
  
  form <- as.formula(sprintf("%s ~ %s", outcome, paste(exposures, collapse=" + ")))
  
  death_28_sex_area <- descrTable(form, 
                                  show.p.trend   = TRUE,  
                                  show.p.overall = TRUE,
                                  byrow          = TRUE, 
                                  show.all       = TRUE,
                                  show.ci        = TRUE,
                                  data           = data) 
  # print(death_28_sex_area)
  
  ## Obtain a list of levels of exposures
  expo.levels <- unlist(map(exposures, ~levels(data[[.]])))   
  
  
  ## convert matrix into a dataframe to work with
  death_28_sex_area_df              <- as.data.frame(death_28_sex_area$descr)               
  colnames(death_28_sex_area_df)    <- c("all", "no.event", "event", "p.trend", "p.overall")  
  death_28_sex_area_df$groups       <- expo.levels
  death_28_sex_area_df$outcome.name <- outcome.label
  
  
  ## Extract prevalence and 95% CI
  death_28_sex_area_df$prev  <- " "
  death_28_sex_area_df$lci   <- " "
  death_28_sex_area_df$uci   <- " "
  
  prev_ci <- str_extract_all(death_28_sex_area_df$event, "\\d+\\.*\\d*")
  
  for (i in 1:length(death_28_sex_area_df$prev)) {
    death_28_sex_area_df$prev[[i]] <- prev_ci[[i]][1]
    death_28_sex_area_df$lci[[i]]  <- prev_ci[[i]][2]
    death_28_sex_area_df$uci[[i]]  <- prev_ci[[i]][3] 
  }  
  # End of For loop
  
  # Assign labels to variables 
    death_28_sex_area_df$strata <- " "
    
    for (e in seq_along(exposures)) {
      
      death_28_sex_area_df$strata <- ifelse(death_28_sex_area_df$groups %in% levels(data[[exposures[[e]]]]), 
                                            exposures[[e]], death_28_sex_area_df$strata)
        
    }
  
  # Select relevant columns
    death_28_sex_area_df <- death_28_sex_area_df [ , c("strata", "groups", "no.event", "event", 
                                                       "prev", "lci", "uci", "outcome.name")]
    
    return(death_28_sex_area_df)
  
  # End of function
}
