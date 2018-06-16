## These functions will store a matrix and, when computed once, it's inverse
## Each time the inverse is called for, the cache is checked, and if the 
## inverse has already been computed, it won't be computed again.

## This function sets up the cache, with functions to set and get the matrix
## and set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function finds the inverse of a matrix, by first checking the cache
## and then computing the inverse if it's not already stored in the cache
## The input has to have the get/set/getinv/setinv as created in the 
## makeCacheMatrix function above.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  temp <- x$get()
  m <- solve(temp)
  x$setinv(m)
  m
}
