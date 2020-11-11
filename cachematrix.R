## The functions below will create and return the inverse of an input matrix. If the 
## inverse has already been calculated, then the functions will get the calculated matrix 
## from the cache.


## This function will create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m = NULL
set = function(y){
  x <<- y
  m <<- NULL
}
get = function() x
setinverse = function(inverse) m <<- inverse
getinverse = function() m
list(set = set, get = get, 
     setinverse = setinverse, 
     getinverse = getinverse)
}


## This function returns the inverse of the matrix returned by the makeCacheMatrix function
## above. If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve will retrieve the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getinverse()
  if(!is.null(inverse)){
    message("getting inversed matrix")
    return(inverse)
  }
  data = x$get()
  inverse = solve(data,...)
  x$setinv(inverse)
  inverse
}