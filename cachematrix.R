## There are two functions- makeCacheMatrix and cacheSolve-, 
##which create and cache the inverse of a matrix.

## makeCacheMatrix is a function which sets and inverts the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x   
  setinv<-function(calculatedinv)inv <<- calculatedinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv= getinv)
}


## This is a function which gets the inverse from the cache.

cacheSolve <- function(x, ...) { ##return matrix of x inverted
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
    data <-x$get()
    inv <- solve(data,...)
    x$setinv(inv)
}

#Checking the functions

test <- matrix(rnorm(25), 5,5)
test1 <- makeCacheMatrix(test)
cacheSolve(test1)
