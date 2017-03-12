## The main purpose of this function is to calculate the inverse of an invertable matrix.  If the inverse hadn't been changed 
## and the inverse had already been claculted it returns the vale from the cache instead of calculating it again. 

##The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to 
##set the value of the Matrix
##get the value of the Matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cachen

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
        ## Return a matrix that is the inverse of 'x'
}
