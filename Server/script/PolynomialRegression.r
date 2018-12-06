source('Server/script/GaussJordan.r')

getPolString <- function(unknowns){
  polString = "function(x)"
  for(i in 1:length(unknowns)){
    if(i == 1){
      polString = paste(polString, unknowns[i], "+", sep=" ")
    }
    else{
      polString = paste(polString, unknowns[i], "* x^", sep = " ")
      polString = paste(polString, i-1, sep="")
      if(i != length(unknowns)){
        polString = paste(polString, "+", sep = " ")
      }
    }
  }
  return (polString)
}
getVectorMatrix <- function(x, y, degree){
  vectorOfCoeff = c() #Storage of the coefficients
  vectorOfRHS = c() #Storage of the RHS  
  for(i in 0:(2*degree)){
    vectorOfCoeff = c(vectorOfCoeff,sum(x^i)) #Store the Summation of Xi^0 up to Xi^n 
    if(i != (2*degree)){
      vectorOfRHS = c(vectorOfRHS, sum(x^i*y)) #X]Store summation of (Xi*Yi)
    }
  }
  vectorOfMatrix = c()  #Vector for the actual creation of matrix
  for(i in 1:(degree+1)){ #Rows
    for(j in 1:(2+degree)){ #Cols
      if(j != (degree+2)){  #If not yet in the column of the RHS
        vectorOfMatrix = c(vectorOfMatrix, vectorOfCoeff[j+(i-1)])
      } else if(j == degree+2){
        vectorOfMatrix = c(vectorOfMatrix, vectorOfRHS[i])    
      }
    }
  }
  return (vectorOfMatrix)
}
PolynomialRegression <- function(degree, listOfXY){
  if(degree < 1 || length(listOfXY[[1]]) != length(listOfXY[[2]])){ #Return NA if degree is less than 1 or the length of 2 vectors is not equal
    return (NA)
  }

  x = listOfXY[[1]]
  y = listOfXY[[2]]

  if(length(x) < degree){
    return(NA)
  }
  
  vectorOfMatrix = getVectorMatrix(x, y, degree) #Get the vector for the matrix
  actualMatrix = matrix(vectorOfMatrix, degree+1, degree+2, byrow = TRUE)
  retList = list(augcoeffmatrix=actualMatrix) #List that contains the necessary elements needed
  unknowns = GaussJordanFunction(actualMatrix)  #Find the value of unknown
  retList = c(retList, list(unknowns = unknowns))
  polString = getPolString(unknowns)
  retList = c(retList, list(polynomial_string=polString))
  polFunc = eval(parse(text = polString))
  retList = c(retList, list(polynomial_function=polFunc))
  return (retList)
}