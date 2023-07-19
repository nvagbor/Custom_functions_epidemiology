
# Function prepares data for a linear plot 

# Aims
## 1. Calculate proportions of some variables (outcome_vars) by groups of another variable (x_var_cat)
## 2. x_var_cat is a continuous variable (x_var_cont) that has been categorised 
## 3. The proportions of outcome_var should be standardised



Make_LineData_Prop <- function(
    
  dataset, 
  group_var,                # Separate plots will be produced for this group (e.g., me and women)
  outcome_var, 
  outcome_var_lab = NULL, 
  x_var_cat,                # Variable to be plotted on the x axis (x_var_cat is the categorical form of x_var_cont)
  x_var_cont,               # Continuous form of x_var_cat
  adjustment_variables = NULL, 
  n_decimal = 1
  
){ 
  
  
  if(!is.null(outcome_var_lab)) {
    
    if (length(outcome_var_lab) != length(outcome_var)) {
      
      stop("Length of `outcome_var` and `outcome_var_lab` must be the same")
      
    } else{
      
      outcome_var_lab <- outcome_var
      
    }
    
  }  
  
  
  group_var_levels <- levels(as.factor(dataset[[group_var]]))
  
  
  df_prop <- list()
  
  for (k in group_var_levels){
    
    
    df_prop[[k]] <-
      
      purrr::map2_dfr( #Rowbind and prepare for plotting --
        
        .x = outcome_var,
        .y = seq_along(outcome_var),
        ~DirectStandardisation::adjprop(
          data              = dataset %>% filter(.data[[group_var]] == k) %>% as.data.frame(.), 
          outcome_vars      = .x, 
          categorical_vars  = x_var_cat, 
          adjustment_vars   = adjustment_variables) %>%  
          
          `row.names<-`(NULL) %>%
          
          select(
            -dplyr::contains(".N.Var1"), 
            -dplyr::contains("variance"), 
            -dplyr::contains(".N.Freq")
          ) %>%  
          
          setNames(gsub(pattern = paste0(.x, "."), replacement = "" , x = names(.) )) %>% 
          dplyr::rename(
            "strata"   = levels, 
            "var_name" = variable) %>% 
          dplyr::mutate(
            
            strata_mean = dataset %>% 
                           dplyr::group_by(.data[[x_var_cat]]) %>%
                           dplyr::summarise(mean_expo = mean(.data[[x_var_cont]], na.rm = TRUE), .groups = "drop") %>% 
                           dplyr::pull(mean_expo)
          ) %>% 
          
          # You might think the line below is not necessary but don't touch it
          setNames(sub(pattern = "\\.", replacement = "_" , x = names(.) )) %>%
          
          tidyr::pivot_longer(
            cols      = !c(var_name, strata, strata_mean),
            names_to  = c(".value", "outcome_var_level"), 
            names_sep = "_"
          ) %>%

          dplyr::mutate(
            LCI_prop     = proportion - 1.96*se,
            UCI_prop     = proportion + 1.96*se,
            outcome_var  = outcome_var_lab[.y],
            group_level  = k
          )

      ) 
    
  }
  
  
  return(df_prop)
  
}
