##Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse.
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.
##cacheSolve takes output of makeCacheMatrix function and returns the inverse of matrix, if inverse is stored in cache returns cached values


## makeCacheMatrix function
## input is invertable matrix
## returns list of function that is used as an input to cacheSolve function
 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } #end set function
  get <- function() x    
  setinverse <- function(solve) m <<- solve  #calculates inverse
  getinverse <- function() m                 
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


##cacheSolve
  ## input is output of makeCacheMatrix
  ## returns inverse of matrix x
  
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
