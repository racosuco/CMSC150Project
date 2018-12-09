initMatrix <- function(vecOfVar, vecOfSupply, vecOfDemand){ 
  RHS = c(vecOfDemand, vecOfSupply, 0)
  lastRow = c(vecOfVar, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  tempVec = c(-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0, 0,	0,	0,	0,	0,	0,	0,	-180,
            0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	-80,
            0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	-200,
            0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	-160,
            0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	-220,
            1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	310,
            0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	260,
            0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	280,
            10,	8,	6,	5,	4,	6,	5,	4,	3,	6,	3,	4,	4,	5,	9,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0)
  augcoeffmatrix = matrix(tempVec, nrow = 9, ncol=25,byrow = TRUE)
  augcoeffmatrix[,25] = RHS
  augcoeffmatrix[9, ] = lastRow
  colnames(augcoeffmatrix) = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z", "RHS")
  rownames(augcoeffmatrix) = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z")
  retList = Simplex(augcoeffmatrix)
  return(retList)
}
lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}
checkCondition <- function(x){
  x = x[-length(x)]
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

getPivotIndex <- function(x){
  denominator = x[length(x)]
  x = x[-length(x)]
  retVal = which.max(x/denominator)
  return(retVal)
}
getResults <- function(resultMatrix){
  answers = c()
  for(i in 1:15){
    index = which(resultMatrix[1:8, i] == 1)
    if(length(index) == 1){
      answers = c(answers, resultMatrix[index[1], 25])
    } else{
      answers = c(answers, 0)
    }
  }
  return(answers)
}

Simplex <- function(augcoeffmatrix){
  matrixIterations = list()
  while(checkCondition(augcoeffmatrix[,ncol(augcoeffmatrix)])){
    matrixIterations = lappend(matrixIterations, augcoeffmatrix)
    tempVector = augcoeffmatrix[,ncol(augcoeffmatrix)]
    tempVector = tempVector[-length(tempVector)]
    pivotRowIndex = which.min(tempVector)
    pivotColumnIndex = getPivotIndex(augcoeffmatrix[pivotRowIndex,])
    augcoeffmatrix[pivotRowIndex,] = augcoeffmatrix[pivotRowIndex,]/augcoeffmatrix[pivotRowIndex, pivotColumnIndex]
    for(j in 1:nrow(augcoeffmatrix)){
      if(pivotRowIndex == j){
        next
      }
      subvector = augcoeffmatrix[j,pivotColumnIndex] * augcoeffmatrix[pivotRowIndex, ] #Vector to be subtracted to the main matrix
      augcoeffmatrix[j, ] = augcoeffmatrix[j, ] - subvector #Actual subtraction
    }  
  }
  while(checkCondition(augcoeffmatrix[nrow(augcoeffmatrix),])){
    matrixIterations = lappend(matrixIterations, augcoeffmatrix)
    pivotColumnIndex = which.min(augcoeffmatrix[nrow(augcoeffmatrix),1:(ncol(augcoeffmatrix)-1)])
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
  matrixIterations = lappend(matrixIterations, augcoeffmatrix)
  vectorValues = getResults(augcoeffmatrix)
  answer = augcoeffmatrix[9,25] * -1
  retList = list(answer = answer, vectorValues =  vectorValues, matrixIterations = matrixIterations, resultMatrix = augcoeffmatrix)
  return(retList)
}