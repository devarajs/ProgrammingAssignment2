
## The makeCacheMatrix function creates a list of functions to get and set
## a matrix. It also creates the functions to get and set the inverse of a matrix.
## The cacheResolve function checks to see if the inverse of the matrix is already
## cached, if not it creates the inverse and stores it in the parent environment so
## the state can be maintained across function invocations.


## makeCacheMatrix is a function that takes matrix as a parameter and returns a list
## functions to set or get values. This function creates a new function which accesses
## the variable in the parent environment to store the matrix and the inverse of
## the matriX. The function returns a list of funtions to set and get the matrix
## and the inverse of the matrix.
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) m <<- matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## This function takes a list and uses the functions in the list to
## get the inverse of the matrix from the cache if it already exists and return
## the inverse. If the inverse if not cached, it creates the inverse of the matrix
## and stores it the  parent environment so when the function is called again,
## it can retrieve it from the from the parent environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
  

