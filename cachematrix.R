## this function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function ()x
  
  setinv <- function(solveMatrix) inv <<- solveMatrix
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## this function computes the inverse of the special "matrix" returned
## by MakeCacheMatrix 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("inverse is cached")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  
  x$setinv(inv)
  
  return(inv)
}
