## These funtions alocate space in memory and a matrix value to
## return its' inverse.
## Written as part of Coursera Data Science: R Programming

## This function takes the original matrix and creates a matrix
## object to cache its' inverse.
## Inve will hold the value of matrix inverse, it is initialized as
## NULL and will be reset as NULL if there is a new matrix.
## X will hold the value of matrix in parent enviroment.

makeCacheMatrix <- function(x = matrix()) {
  Inve <- NULL
  set <- function(y) {
    x <<- y
    Inve <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inve <<- inverse
  getinverse <- function() Inve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)       ##Set as a list to reffer functions with $ operator
}

## This function computes the inverse of the matrix returned by previous function

cacheSolve <- function(x, ...) {
  Inve <- x$getinverse()
  if(!is.null(Inve)) {        ## If it has already been calculated
    message("getting cached data")
    return(Inve)              ##Retrives inverse of 'x' from cache
  }
  data <- x$get()
  Inve <- solve(data, ...)
  x$setinverse(Inve)
  Inve        ## Return a matrix that is the inverse of 'x'
}