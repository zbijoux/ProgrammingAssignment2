### Caching the inverse of a Matrix###
## We are assuming the matrix supplied is always invertible.

## makeCacheMatrix creates a Matrix,which is a list containing functions :
## 1.setM-set the value of the Matrix
## 2.getM-get the value of the Matrix
## 3.setIM-set the value of the inverse of the Matrix
## 4.getIM-get the value of the inverse of the Matrix

### This function makeCacheMatrix creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(M = matrix()){
        IM <- NULL ## an empty matrix to store the inverse of the matrix
        
       ## setting the value of the Matrix 
        setM <- function(A){
           M <- A
          IM <<- NULL  ## IM is the inverse of the matrix M
}

     ## getting the value of the Matrix
       getM <- function() M

    ## setting the value of the inverse Matrix
       setIM <- function(I) IM <<- I

   ## getting the value of the inverse Matrix
       getIM <- function() I 
       list(setM=setM, getM=getM, 
           setIM=setIM, getIM=getIM)
}


### The next function calculates the inverse of the special "matrix" return by makeCacheMatrix above. 
 
      
cacheSolve <- function (M,...){
      
      ## Checks if the inverse has already been calculated
        
      IM <- M$getIM()
      
      ## If so gets the inverse from the cache and skip computation
        if(!is.null(IM)){
             message("getting cached data")
             return(IM)
}
    ## otherwise, calculates the inverse and set the value of the inverse in the cache via the setIM function

      data <- M$get()
        IM <- solve(data, ...)
        M$setIM(IM)
        IM
}
