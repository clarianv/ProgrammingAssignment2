## This library enables calculating a value of a matrix, 
# and cache them for possible future use 
# to avoid recalculation of the same data
#
# Example:
# d1 <- makeCacheMatrix(matrix(6:15,nrow=2)) -- here it creates the special matrix 
# cacheSolve(d1) -- here it checks that there is no cached data yet, so it calculates the inverse an save the value
# cacheSolve(d1) -- here it checks that there is cached data, and just returns the previously saved value





#creates a special "matrix", which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## gets the inverse of a matrix by using the special matrix above, where
#1. it tries to retreive a previously calculated inverse, or if none
#2. calculates the inverse, then save it for possible future use
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
