## makeCacheMatrix() function is the parent function with set(), get(), getInv(), setInv(), as its child functions within it and it makes a cache of inverse of the matrix
## and the cacheSolve() function computes the inverse of the matrix with the help of inbuilt function "solve()"


## This function makes a cache of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())  {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInv <- function(inv1) {inv <<- inv1}
  getInv <- function() {inv}
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the matrix if it has not already been computed and if it has already been computed, then it gets it from the cached data

cacheSolve <- function(x, ...){
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv                   ## Return a matrix that is the inverse of 'x'
}


