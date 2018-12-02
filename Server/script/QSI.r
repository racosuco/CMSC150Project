source('Server/script/GaussJordan.r')

lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

GetCoefficient <- function(x, y){
  loopLength = length(x)
  retList = list()
  for(i in 1:loopLength){
    tempVector = c(x[i]*x[i], x[i], 1, y[i])
    if(i == 1 || i == loopLength){
      retList = lappend(retList, tempVector)
    } else{
      retList = lappend(retList, tempVector, tempVector)
    }
  }
  
  for(i in 1:loopLength){
    if(i == 1 || i == loopLength){
      next
    } else{
      tempvector = c(2*x[i], 1, 0, -2*x[i], -1, 0)
      retList = lappend(retList, tempvector)
    }
  }
  retList = lappend(retList, c(1, 0))
  return(retList)
}

GetResultantMatrix <- function(augMatrix, listOfCoeff, x){
  loopLength = length(listOfCoeff)
  numOfTCD = (length(x) - 1) * 2
  count = 0
  count2 = 0
  for(i in 1:loopLength){
    tempVector = listOfCoeff[[i]]
    if(i <= numOfTCD){
      for(j in 1:length(tempVector)){
        if(j == length(tempVector)){
          augMatrix[i, ncol(augMatrix)] = tempVector[j]
        } else{
          augMatrix[i, j+count] = tempVector[j]
        }
      }
      if(i %% 2 == 0){
        count = count + 3 
      }
    } else{
      if(i == loopLength){
        augMatrix[i, 1] = tempVector[1]
      } else{
        for(j in 1:length(tempVector)){
          if(j == length(tempVector)){
            augMatrix[i, ncol(augMatrix)] = tempVector[j]
          } else{
            augMatrix[i, j+count2] = tempVector[j]
          }
        }
        count2 = count2 + 3 
      }
    }
  }
  return(augMatrix)
}

GetFuncList <- function(resultantList){
  loopLength = length(resultantList)
  polStringList = list()
  for(i in seq(1, loopLength, 3)){
    tempString = paste("function(x)", resultantList[i], "* x^2 +", resultantList[i+1], "* x +", resultantList[i+2])
    polStringList = lappend(polStringList, tempString)
  }
  return(polStringList)
}

ConvertToFunctions <- function(polStringList){
  polFuncList = list()
  for(i in 1:length(polStringList)){
    tempFunc = eval(parse(text = polStringList[i]))
    polFuncList = lappend(polFuncList, tempFunc)
  }
  return(polFuncList)
}

QSI <- function(listOfXY){
  x = listOfXY[[1]]
  y = listOfXY[[2]]
  augMatrix = matrix(data=0, nrow=(length(x)-1)*3, ncol=(length(x)-1)*3+1)
  
  if(length(x) != length(y)){
    return (NA)
  }
  
  listOfCoeff = GetCoefficient(x, y)
  resultantMatrix = GetResultantMatrix(augMatrix, listOfCoeff, x)
  resultantList = GaussJordanFunction(resultantMatrix)
  polStringList = GetFuncList(resultantList) 
  polFuncList = ConvertToFunctions(polStringList)
  return(polFuncList)
}