## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix construct a matrix object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) inv <<- Inv
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve function will check if the matrix is already had it inverse or not
## if not then the function will continue to caculate the inverse and store the inverse matrix in the object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)){
    print("Returning cached Inverse of matrix x:")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
