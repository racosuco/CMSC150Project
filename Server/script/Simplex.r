#eqs = c(7, 11, 1, 0, 0, 0, 0, 77, 10, 8, 0, 1, 0, 0, 0, 80, 1, 0, 0, 0, 1, 0, 0, 9, 0, 1, 0, 0, 0, 1, 0, 6, -150, -175, 0, 0, 0, 0, 1, 0)
#exMatrix = matrix(eqs, nrow = 5, ncol = 8, byrow = T)

checkCondition <- function(x){
  if(length(x[x<0]) > 0){
    return(TRUE)
  }
  return(FALSE)
}
getPrIndex <- function(tempACM, pivotColumnIndex){
  TRVector = c()
  for(i in 1:nrow(tempACM)){
    if(tempACM[i, pivotColumnIndex] == 0){
      ans = 99999
    } else{
      ans = tempACM[i, ncol(tempACM)]/tempACM[i, pivotColumnIndex]
    }
    TRVector = c(TRVector, ans)
  }
  retValue = min(TRVector[which(TRVector > 0)])
  prIndex = match(retValue, TRVector)
  return(prIndex)
}
Simplex <- function(augcoeffmatrix){
  while(checkCondition(augcoeffmatrix[nrow(augcoeffmatrix),])){
    pivotColumnIndex = which.min(augcoeffmatrix[nrow(augcoeffmatrix),])
    tempACM = augcoeffmatrix[-nrow(augcoeffmatrix),]
    pivotRowIndex = getPrIndex(tempACM, pivotColumnIndex)
    augcoeffmatrix[pivotRowIndex, ] = augcoeffmatrix[pivotRowIndex, ]/augcoeffmatrix[pivotRowIndex,pivotColumnIndex]
    for(j in 1:nrow(augcoeffmatrix)){
      if(pivotRowIndex == j){
        next
      }
      subvector = augcoeffmatrix[j,pivotColumnIndex] * augcoeffmatrix[pivotRowIndex, ] #Vector to be subtracted to the main matrix
      augcoeffmatrix[j, ] = augcoeffmatrix[j, ] - subvector #Actual subtraction
    }
  }
  return(augcoeffmatrix)
}