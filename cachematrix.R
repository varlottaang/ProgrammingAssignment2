# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

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
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache
# via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# Example
# > prova <-matrix(c(6,3,5,1,6,8,6,9,2), 3,3)
# > inverso <-makeCacheMatrix(prova)
# > cacheSolve(inverso)
#             [,1]        [,2]        [,3]
#[1,]  0.16806723 -0.12885154  0.07563025
#[2,] -0.10924370  0.05042017  0.10084034
#[3,]  0.01680672  0.12044818 -0.09243697
# > cacheSolve(inverso)
# getting cached data

