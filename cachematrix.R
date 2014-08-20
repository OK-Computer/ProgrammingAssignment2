
## *****makeCacheMatrix function*****
## This function creates a special "matrix" object that can cache its inverse
## This is really a list of the following functions:
## 1. set = sets the value of the matrix
## 2. get = gets the value of the matrix
## 3. setSolve = sets the value of the inverse of the matrix 
##    (using the 'solve' function)
## 4. getSolve = gets the value of the inverse of the matrix 
##    (using the 'solve' function)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## *****cacheSolve function*****
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.  

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  
  ## Checking to see if the inverse has been calculated.
  ## If the inverse has already been calculated (and the matrix has 
  ## not changed), then the cacheSolve should retrieve the inverse
  ## from the cache.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise, the function calculates the inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
