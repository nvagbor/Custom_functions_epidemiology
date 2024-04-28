# This creates a vector sequence where the current index is 2*previous index
# This is important when plotting on the log scale
doubling_sequence <- function(from, to){
  if(from <= 0){stop("The minimum value in the sequence must be >=0")}
  
  double_seq <- c(from)
  while(from*2 <= to){
    ii <- from*2
    double_seq <- c(double_seq, ii)
    }
  return(double_seq)
  }
