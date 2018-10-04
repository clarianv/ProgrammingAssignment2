## This library enables calculating a value of a matrix, 
# and cache them for possible future use 
# to avoid recalculation of the same data
#
# Example:
# d1 <- makeCacheMatrix(matrix(6:15,nrow=2)) -- here it creates the special matrix 
# cacheSolve(d1) -- here it checks that there is no cached data yet, so it calculates the mean an save the value
# cacheSolve(d1) -- here it checks that there is cached data, and just returns the previously saved value





#creates a special "matrix", which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the mean
#4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## gets the mean of a matrix by using the special matrix above, where
#1. it tries to retreive a previously calculated mean, or if none
#2. calculates the mean, then save it for possible future use
cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
