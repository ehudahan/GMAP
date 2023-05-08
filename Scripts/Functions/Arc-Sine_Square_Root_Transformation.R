#######################################
# Arc-Sine Square Root Transformation #
#######################################

# Arc Sine Square Root Transformation
AST <- function(x) {
  return(sign(x) * asin(sqrt(abs(x))))
}