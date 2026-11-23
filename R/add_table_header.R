## Function takes a dataframe containing results by categories of a group variable
## Without variable label 
## Adds the variable label above the variable categories
## The data must containing a column of variable to be used as group heading and 
## a column containing the names of group levels 

## Extra_space argument adds an extra space with of missing value above the inserted headers

## This is important for producing a table for reporting 
## Or formating data for producing forest plot using Jasper 


## Dependencies: Tidyverse, purrr


add_table_header <- function(data,
                             group_var,
                             group_level_var,
                             extra_space = FALSE) {
  
  group_vars <- names(
    tidyselect::eval_select(rlang::enquo(group_var), data)
  )
  
  level_var <- rlang::as_string(rlang::ensym(group_level_var))
  
  # Create an empty row retaining column types
  empty_row <- data[NA_integer_, , drop = FALSE]
  
  # Identify where each group begins
  group_start <- purrr::map_int(seq_len(nrow(data)), function(i) {
    
    if (i == 1) return(1L)
    
    changed <- which(
      !purrr::map_lgl(group_vars, function(v) {
        identical(data[[v]][i], data[[v]][i - 1])
      })
    )
    
    if (length(changed)) min(changed) else NA_integer_
  })
  
  # Insert headings before each new group
  tab_df <- purrr::map_dfr(seq_len(nrow(data)), function(i) {
    
    start <- group_start[i]
    
    if (is.na(start)) return(data[i, ])
    
    headers <- purrr::map_dfr(
      seq.int(start, length(group_vars)),
      function(j) {
        header <- empty_row
        header[[level_var]] <- as.character(data[[group_vars[j]]][i])
        header
      }
    )
    
    if (extra_space && start == 1 && i > 1) {
      headers <- dplyr::bind_rows(empty_row, headers)
    }
    
    dplyr::bind_rows(headers, data[i, ])
  })
  
  tab_df %>%
    dplyr::select(-dplyr::all_of(group_vars))
}






