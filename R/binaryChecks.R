


binaryChecks <- function(x, method){
  if(length(unique(x)) > 2){
    stop(paste(method, "only works for binary outcomes at this time"))
  }
}
