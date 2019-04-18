##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL #setting the inverse matrix ,i , to NULL 
  set <- function(y = matrix()) {
    x <<- y #set the matrix, x, to a new matrix, y, and resets the inverse, i, to NULL
    i <<- NULL
  }
  get <- function() x #returns the matrix, x
  setinverse <- function(inverse) i <<- inverse  #sets the inverse, i, to inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #returns the 'special vector' containing all of the functions just defined
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.If the 
#inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
