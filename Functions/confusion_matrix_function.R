confusion <- function(matrix){
  TN <- matrix[1, 1]
  FN <- matrix[2, 1]
  FP <- matrix[1, 2]
  TP <- matrix[2, 2]
  
  sens <- TP / (TP + FN)  # sensitivity 
  spec <- TN / (TN + FP) # specificity
  
  return(list(sensitivity=sens, specificity=spec))
}