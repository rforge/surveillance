################################################################################
# FORMULA FUNCTION
################################################################################
# Function for writing the good formula depending on timeTrend,
# and factorsBool

formulaGLMDelay <- function(timeBool=TRUE,factorsBool=FALSE,delay=FALSE,outbreak=FALSE){
  # Description
  # Args:
  #     populationOffset: ---
  # Returns:
  #     Vector of X
  
  # Smallest formula
  formulaString <- "response ~ 1"
  
  # With time trend?
  if (timeBool){
    formulaString <- paste(formulaString,"+wtime",sep ="")}
  
 
  
  # With factors?
  if(factorsBool){
    formulaString <- paste(formulaString,"+as.factor(seasgroups)",sep ="")}
  #   # With delays?
  if(delay){
    formulaString <- paste(formulaString,"+as.factor(delay)",sep ="")}  
  if(outbreak){
    formulaString <- paste(formulaString,"+f(outbreakOrNot,model='linear', prec.linear = 1)",sep ="")}  
  # Return formula as a string
  return(formulaString) 
}
################################################################################
# END OF FORMULA FUNCTION
################################################################################