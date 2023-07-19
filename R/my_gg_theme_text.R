# My custom functions for ggplot2 theme

# 1. Format aspects of text 

my_gg_theme_text <- function(font_style          = "Times",
                             axis.text.size      = 13, 
                             axis.text.x.hvjust  = c(0,0),      # Numeric vector of length two. 1 = Hjustified; 2 = Vjustification
                             axis.text.y.hvjust  = c(-0.1,0), 
                             axis.title.size     = 14,
                             axis.title.x.hvjust = c(0,-1), 
                             axis.title.y.hvjust = c(0, 8),
                             axis.title.xy.angle = c(0, 90),    # Vector of length two. 1 = X-axis; 2 = Y-axis
                             ...) {
  
  theme(text         = element_text(family = font_style),
        axis.text.x  = element_text(size  = axis.text.size, 
                                    vjust = axis.text.x.hvjust[[2]]),
        
        axis.text.y  = element_text(size  = axis.text.size, 
                                    hjust = axis.text.y.hvjust[[1]]), 
        
        axis.title.x = element_text(size  = axis.title.size, 
                                    vjust = axis.title.x.hvjust[[2]]), 
        
        axis.title.y = element_text(size  = axis.title.size, 
                                    angle = axis.title.xy.angle[[2]], 
                                    vjust = axis.title.y.hvjust[[2]]))
  
  
}


# 2. create custom X and Y tick lines and axis ticks after using theme_void()

my_theme_axis_tick_lines <- function(col.axis           = "black", 
                                     size.axis          = 0.6, 
                                     linetype.axis      = "solid", 
                                     size.ticks         = 0.6, 
                                     length.ticks       = 0.25, 
                                     length.ticks.unit  = "cm") {
  
  theme(axis.line         = element_line(colour = col.axis, 
                                         size = size.axis, 
                                         linetype = linetype.axis), 
        
        axis.ticks        = element_line(size = size.ticks),
        axis.ticks.length = unit(length.ticks, length.ticks.unit))
  
}


#3. Custom formatting of the plot margins 

my_theme_plot_margin <- function(margin.width = c(1,1,3,3.5),
                                 
                                 margin.width.unit   = "cm") {
  
  theme(plot.margin = unit(margin.width, margin.width.unit))
  
  ##Orientation: Top, right, bottom, left
}


#4. Axis breaks for log_transformed axis
#========================================

# From Stack overflow: 
# https://stackoverflow.com/questions/14255533/pretty-ticks-for-log-normal-scale-using-ggplot2-dynamic-not-manual

# Edited to take custom range of values for the axes

base_breaks <- function(min.val = NULL, 
                        max.val = NULL, 
                        n = 10){
  
  if(missing(min.val) & missing(max.val)) {
    #If custom range is missing, obtain from data 
    
    function(x) {
      axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
    
  }
  
  if(missing(min.val) & !missing(max.val)) {
    stop("Please provide both minimum and maximum value for axis range")
    }
  
  function(x) {
    axisTicks(log10(c(min.val, max.val)), log = TRUE, n = n)
  }
}




















