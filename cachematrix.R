

## Coding by Matthias Ho for 
## Coursera course : R Programming
## Peer Assessments /Programming Assignment 2
## 27 April 2014
## matthiasho@yahoo.com

## Testing with small matrix i.e. 4 by 4
## Me =  matrix( c(1, 9, 11, 5, 12,2,6,13,16,7,3,14,8,15,10,4), nrow = 4, ncol=4,byrow=TRUE)

## Generate a huge matrix i.e. 2200 by 2200 for testing
MMEE =matrix ( c(sample(1:9999, 4840000, replace=T)),nrow=2200,ncol=2200,byrow=TRUE)


## makecacheMatrix will a "cached" version of a matrix.
## For instance, after building the huge MMEE matrix as above, one can made a "cached" version of 
## this matrix by this command:
## Big <- makeCacheMatrix(MMEE)
## Big is now the cached version of the matrix MMEE
## This the essentially a list consisting of 4 functions: setmatrix, getmatrix, setinverse and getinverse
## Big can then be used to call the cacheSolve function by this command: 
## cacheSolve(Big)
## First time calcution of inverse of the huge matrix will take some number of seconds
## Subsequent calcution of inverse of the same huge matrix will obtain result from cache and 
## return the result within 1 second


makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  getmatrix <- function() 
    x
  
  setinverse <- function(inverse)
    m <<- inverse
  
  getinverse <- function() 
    m
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}





## This function calculate the inverse of a matrix for the first time.
## For subsequent calculate of the same matrix, the cache result is return, thereby saving computation
## time.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data of Inverse of the matrix!")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
