## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setInver <- function(solveMatrix) inv <<- solveMatrix
  getInver <- function() iv
  list(set = set, get = get, setInver = setInver, getInver= getInver)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iv <- x$getInver()
  if(!is.null(iv)){
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data)
  x$setInver(inv)
  iv      
}

