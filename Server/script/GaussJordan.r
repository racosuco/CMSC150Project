getIndex <- function(augcoeffmatrix, pivotelement, counter){  #Returns the index of pivotelement
  for(i in counter:nrow(augcoeffmatrix)){
    if(pivotelement == abs(augcoeffmatrix[i, counter])){
      return (i)
    }
  }
}
swapRow <- function(augcoeffmatrix, i, pivotrowindex){  #Swap rows to implement pivoting
  tempmatrix = augcoeffmatrix
  augcoeffmatrix[pivotrowindex, ] = tempmatrix[i, ]
  augcoeffmatrix[i, ] = tempmatrix[pivotrowindex, ]
  return (augcoeffmatrix)
}
GaussJordanFunction <- function(augcoeffmatrix){
  for(i in 1:nrow(augcoeffmatrix)){
    if(i != nrow(augcoeffmatrix)){  #No need to pivot at last row
      pivotelement = max(abs(augcoeffmatrix[i:nrow(augcoeffmatrix), i])) #Get max abs value in the current column 
      if(pivotelement == 0){  #Stop if there would be a division by 0
        return (NA)
      }
      pivotrowindex = getIndex(augcoeffmatrix, pivotelement, i) #Get the index of the pivot element
      augcoeffmatrix = swapRow(augcoeffmatrix, i, pivotrowindex)  #Swap rows
    }
    augcoeffmatrix[i, ] = augcoeffmatrix[i, ]/augcoeffmatrix[i,i] #Normalize the matrix
    for(j in 1:nrow(augcoeffmatrix)){
      if(i==j){ #Skip self when making the elements above and below it zero
        next
      }
      subvector = augcoeffmatrix[j,i] * augcoeffmatrix[i, ] #Vector to be subtracted to the main matrix
      augcoeffmatrix[j, ] = augcoeffmatrix[j, ] - subvector #Actual subtraction
    }
  }
  x=as.vector(augcoeffmatrix[,ncol(augcoeffmatrix)]) #Vector of unknowns from X1 to Xn
  return (x)
}
