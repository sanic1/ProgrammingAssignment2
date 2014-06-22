## The first function is for creating an object, 
## which can contain inversed matrix. 
## Created object is used in the second function.

## Here we create object to use for retrieving cached inversed matrix for input {x}.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversed) inv <<- inversed
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Here we try to retrieve inversed matrix if it exists for object {x}, 
## created using upper function. If it does - we just use already calculated value.
## Else we calculate inversed matrix using {solve} function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinv(inv)
  inv
}
