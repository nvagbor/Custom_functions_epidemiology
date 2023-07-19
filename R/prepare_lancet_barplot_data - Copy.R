
## Function will prepare data to facilitate plotting of barplots WITH ticks inbetween bars
##
## See example from this 

barplot_data <- function(dataset, 
                         region_var,
                         include_area_df = FALSE,
                         dataset_area = NULL,
                         area_var = NULL ){  
  
  ## Preliminary checks---
  
  # if(length(dataset[[region_var]]) != 10) stop("Length of `dataset` should be 10")
  # 
  # if(include_area_df){
  #   if(length(dataset_area[[area_var]]) != 2) stop("Length of `dataset_area` should be 2")
  # }
  # 
  
  ## Prepare data for REGION ---
  
  # Get region levels
  if (is.factor(dataset[[region_var]])) {
    region_level <- dataset %>% 
                      dplyr::pull({{region_var}}) %>% 
                             as.character(.) %>% 
                             unique(.)
                      
  }else
    
  {region_level <- unique(dataset[[region_var]])}
  
  # Obtains required number of NA levels 
  nalevel <- seq_len(length(region_level))
  nalabel <- paste0("nalevel_", nalevel)
  
  # Insert NA levels to region data and maintain ordering 
  
  dataset[ , region_var] <- factor(dataset[[region_var]], levels = region_level)
  dataset_split <- split(dataset, f=dataset[[region_var]], drop = FALSE)
  
  
  df_region_split <- vector(mode = "list", length = length(dataset_split))
  
  for (i in seq_along(dataset_split)) {
    
    df_region_split[[i]] <- dataset_split[[i]] %>% 
      dplyr::add_row(
        .before = 1,
        {{region_var}} := nalabel[[i]])
    
  }
  
  df_region_na <- dplyr::bind_rows(df_region_split)
  
  # Add empty row at the end of the data
  df_region_na <- df_region_na %>% 
    dplyr::add_row(
      .after =  nrow(.), 
      {{region_var}} := paste0("nalevel_", length(nalevel)+1)
    )
  
  # Convert region variable to factor 
  # Main original ordering 
  region_na_levels <- df_region_na %>% dplyr::pull({{region_var}})
  df_region_na <- df_region_na %>% 
    dplyr::mutate({{region_var}}:= factor(.data[[region_var]], levels = region_na_levels))
  
  # Get label for x-axis ticks 
  bar_labels_region <- as.character(df_region_na[[region_var]])
  bar_labels_region[grepl("nalevel", bar_labels_region)] <- ""
  
  #Merge labels to dataset 
  df_region_na$x_label <- bar_labels_region
  
  
  # Format AREA dataframe--- 
  
  if (include_area_df) {
    
    # Get region levels
    if (is.factor(dataset_area[[area_var]])) {
      area_level <- dataset_area %>% 
                        dplyr::pull({{area_var}}) %>% 
                               as.character(.) %>% 
                               unique(.)
    }else
      
    {area_level <- unique(dataset_area[[area_var]])}
    
    area_level1 <- as.character(area_level)[1]
    area_level2 <- as.character(area_level)[2]
    
    area_labels_temp <- 
      c(# Add labels for area to merge with region dataset
        "nalevel_12", 
        area_level1, 
        "nalevel_13", 
        area_level2, 
        "nalevel_14"
      )
    
    #Split data
    dataset_area[ , area_var] <- factor(dataset_area[[area_var]], levels = area_level)
    dataset_split2 <- split(dataset_area, f=dataset_area[[area_var]], drop = FALSE)
    
    df_area_split <- vector(mode = "list", length = length(dataset_split2))
    
    for (i in seq_along(dataset_split2)) {
      
      df_area_split[[i]] <- dataset_split2[[i]] %>% dplyr::add_row(., .before = 1)
      
    }
    
    df_area_na <- dplyr::bind_rows(df_area_split)
    df_area_na <- df_area_na %>% dplyr::add_row(.)
    
    
    ## Get labels for barplots
    area_labels_na <- area_labels_temp
    area_labels_temp[grepl("nalevel", area_labels_temp)] <- ""
    df_area_na$x_label <- area_labels_temp
    df_area_na$area_na <- area_labels_na
    
  }
  
  
  return(list("region_df" = df_region_na, "area_df" = df_area_na))
  
  
}

# Example 
# xyz <- barplot_data(dataset = df_region[[1]], 
#                     region_var = "region", 
#                     include_area_df = TRUE,  
#                     dataset_area = df_area[[1]], 
#                     area_var = "Strata_level")  
