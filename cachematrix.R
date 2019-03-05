## The following functions create a matrix and cache its 
## inverse into a different environment.

## The first function will create a matrix. 
## Set the function to an object to create, for example: 
## matrix<-makeCacheMatrix(matrix(1:4,2,2)). Then call it's elements to 
## view, for example: matrix$get will return the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function will create the inverse of the above function's matrix. Enter the
## object created above into the funtion, for example: cacheSolve(matrix). 

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

## Use the list elements to retrieve the inverse matrix as noted above (matrix$getInv).
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function should retrieve the inverse from the cache.
