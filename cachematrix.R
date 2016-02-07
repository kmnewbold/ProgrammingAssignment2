## The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## I tested these functons using a method suggested by Community TA Gregory Horne:
## I wrote my code in RStudio and ran it in R
## I then created a test matrix in R:
## testmatrix = makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
## I typed testmatrix$get() to return the original matrix
## I then ran cacheSolve(testmatrix) to compute, cache, and return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x = matrix(), ...) {
  inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv) 
    }
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}