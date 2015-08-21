## Set of functions related with matrix setting, and 
## the computation of its inverse.
## Assumming the matrix is a square invertible matrix

## 'makeCacheMatrix': define 4 functions related with a matrix
## Input parameter 'x': must be a numeric matrix
## Output parameter: list containing functions to
##  1. 'set': set the matrix
##  2. 'get': get the matrix
##  3. 'setinv': set the inverse matrix
##  4. 'getinv': get the inverse matrix
## Example: A <- makeCacheMatrix(matrix(runif(9,0,5),3,3))

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL  # Inverse matrix null
  set <- function (y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function () as.matrix(x)
  setinv <- function (A) invmat <<- A
  getinv <- function () invmat
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve': calculate the inverse matrix of A 
## if the original matrix A has not changed.
## Input parameter: an object created with makeCacheMatrix
## Output parameter: the inverse matrix
## Example: cacheSolve(A)

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if (!is.null(invmat)) {
    message("getting cached inverse matrix")
    return(invmat)
  }
  mymatrix <- x$get()
  ## Assumming square matrix and invertible
  invmat <- solve(mymatrix)
  ## Set the 'invmat' in the 'x' object
  x$setinv(invmat)
  ## Return a matrix that is the inverse of 'x'
  invmat        
}
