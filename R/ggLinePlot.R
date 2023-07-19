
# Make Line plots of two numeric variables 

# Plots -- 
#Lines, points, and errorbar (if needed)


# Make geoms --

ggLinePlot <- function(
    dataset, 
    x_var,                       # Variable on the x-axis. Must be continuous 
    y_var,                       # Variable on the y-axis. Must be continuous 
    outcome_group,               # Column name indicating the variable(s) to be plotted on the y-axis
    plot_groupcolor = NULL,      # Color for different outcome groups to be plotted on the y-axis, 
    plot_linetype   = NULL,
    linewidth_scale = 0.7,       # Option to modify linewidth
    plot_pointshape = NULL,
    pointshape_scale = 0.5,
    add_CIs  = FALSE,            # Should confidence intervals be added?
    add_Line = TRUE,             # Lines need to be explicitly turned off
    upper_CI = NULL, 
    lower_CI = NULL, 
    x_limits,
    y_limits,
    x_ticks, 
    x_ticks_labs,                # Character or numeric vector (Same length as x_ticks)
    y_ticks,
    y_ticks_labs,                # Character or numeric vector 
    legend_position = "right", 
    x_label = NULL, 
    y_label = NULL){ 
  
  
  # Recode outcome group var
  if(!is.factor(dataset[[outcome_group]])){
    
    outcome_labs <- unique(dataset[[outcome_group]])
    
    dataset[ , outcome_group] <- factor(dataset[[outcome_group]], levels = outcome_labs)
    
  }
  
  # Group colour settings---
  
  range_color <- c("black", "red", "#9467BD", "#0079F2", "#227487", "#FFCDB9", "#783728")
  
  
  group_color <- 
    if(is.null(plot_groupcolor)){
      
      n_outcome_var <- levels(dataset[[outcome_group]])
      
      range_color[seq_len( length(n_outcome_var) )]
      
    }else{
      plot_groupcolor
    }
  
  
  # Line type settings --
  
  range_linetype <- c("solid", "dashed", "dotted", "dotdash", "longdash")
  
  group_linetype <- 
    
    if(is.null(plot_linetype)){          
      
      n_outcome_var <- levels(dataset[[outcome_group]])
      
      range_linetype[seq_len( length(n_outcome_var) )]
      
    } else{
      plot_linetype 
    }
  
  
  # Shape settings ----
  
  
  # Point types -- 
  range_points <- c(19, 1, 15, 0, 18, 5)
  
  # Checks
  # if(!is.numeric(plot_pointtype)){stop("`plot_pointtype` must be numeric")}
  
  group_pointshape <- 
    if(is.null(plot_pointshape)){ 
      
      n_outcome_var <- levels(dataset[[outcome_group]])
      range_points[ seq_len(length(n_outcome_var)) ]
      
    } else{
      plot_pointshape   
    }
  
  
  
  # Plotting proper -- 
  
  plot_gg <- 
    
    ggplot2::ggplot(
      
      data = dataset,  
      mapping = aes(
        x     = .data[[x_var]], 
        y     = .data[[y_var]], 
        color = .data[[outcome_group]])
    ) +
    
    geom_point(
      mapping = aes(shape = .data[[outcome_group]]), 
      size    = pointshape_scale
    ) 
  
  # Make line --
  if(add_Line){
    
    plot_gg <- 
      
      plot_gg + 
        geom_line(
          mapping   = aes(linetype = .data[[outcome_group]]), 
          linewidth = linewidth_scale, 
          data      = dataset
      ) 
  
  }
  
  
  # Make error bars --     
  if(add_CIs){
    
    plot_gg <- 
      
      plot_gg + 
      geom_errorbar(
        mapping = aes(
          
          x     = .data[[x_var]], 
          y     = .data[[y_var]], 
          ymin  = .data[[lower_CI]], 
          ymax  = .data[[upper_CI]],
          color = .data[[outcome_group]]), 
        
        width  = 0.5, 
        linewidth = linewidth_scale, 
        data = dataset)  
    
  } 
  
  # Themes ---
  
  plot_gg <- 
    
    plot_gg + 
    theme_void() + 
    
    theme(
      
      axis.text.x  = element_text(size = 13, vjust = 0),
      axis.text.y  = element_text(size = 13, hjust = 1), 
      axis.title.x = element_text(size = 14, vjust = -1), 
      axis.title.y = element_text(size = 14, angle = 90, vjust = 8)
      
    ) +
    
    # Plot floating axes --
    
    ## X-axis 
    geom_segment(
      mapping = aes(
        x    = min(x_ticks), xend = max(x_ticks), 
        y    = min(y_limits), yend = min(y_limits)), 
      linewidth = linewidth_scale, 
      color = "black") + 
    
    ## Y-axis
    geom_segment(
      mapping = aes(
        x     = min(x_limits), xend = min(x_limits), 
        y     = min(y_ticks), yend = max(y_ticks)), 
      linewidth = linewidth_scale, 
      color   = "black") +
    
    theme(
      axis.ticks        = element_line(linewidth = linewidth_scale - 0.1),
      axis.ticks.length = unit(0.25, "cm"),  
      # plot.margin       = unit(c(1.5, 1, 1, 1), "cm"), 
      legend.position   = if(!is.null(legend_position)) {legend_position}
    ) +  
    
    
    guides(
      color = guide_legend(
        title      = "", 
        title.theme = element_text(size = 11, face = "bold"),  
        title.hjust = 0,  
        label.theme = element_text(size = 11), 
        label.hjust = 0), 
      
      linetype = "none", 
      shape    = "none"
    ) +
    
    scale_color_manual(
      values  = group_color,
      labels  = levels(dataset[[outcome_group]])) +
    
    scale_linetype_manual(
      values = group_linetype, 
      guide = "none") +
    
    scale_shape_manual(
      values = group_pointshape, 
      guide = "none") +
    
    # Scales -- 
    scale_x_continuous(
      
      limits = x_limits, 
      expand = c(0, 0), 
      breaks = x_ticks, 
      labels = if(!missing(x_ticks_labs)){x_ticks_labs} else{x_ticks}) + 
    
    scale_y_continuous(
      
      limits = y_limits,
      expand = c(0, 0), 
      breaks = y_ticks, 
      labels = if(!missing(y_ticks_labs)){y_ticks_labs} else{y_ticks}) + 
    
    coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off") + 
    
    
    # Axis titles  
    xlab(x_label) + 
    ylab(y_label)
  
  
  return(plot_gg)
  
}