# Calculate RDR using Peto's method from summary data

# Groups   baseline_var   resurvey_var   
# Q1         0.7942553       0.8440132         
# Q2         0.8525696       0.8833527         
# Q3         0.8869064       0.9072919         
# Q4         0.9207248       0.9277410         
# Q5         0.9798476       0.9605694         


# Groups = Quintiles created using `baseline_var`
# baseline_var = baseline exposure or measurement
# baseline_var = baseline exposure or measurement 

rdr_peto <- function(dataset, baseline_var, resurvey_var){
  
    # Checks -- 
    if(length(baseline_var) != length(resurvey_var)) {
      
      stop("`baseline_var` and `resurvey_var` must have the same lengths")
    
      }
    
    # Get range of group-specific means --
    resurvey_range <- range(dataset[[resurvey_var]])
    baseline_range <- range(dataset[[baseline_var]])
    
    # Calculate regression dilution ratio 
    RDR <- (resurvey_range[2] - resurvey_range[1]) / (baseline_range[2] - baseline_range[1])
    
    return(RDR)

}
  