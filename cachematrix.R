## set the value of the matrix

##get the value of the matrix

##set the value of the inverse

##get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the 
##special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  i <- x$getinverse()       ## Return a matrix that is the inverse of 'x'
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i 
}
