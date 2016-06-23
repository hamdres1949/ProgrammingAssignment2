## The first function creates a special matrix where the matrix and its inverse in a list format
## are set so that the inverse be cached. The second one checks if the inverse
## has already been computed and if not, it computes it.

## The imput of the function is a matrix. It converts it into a
##list where both the matrix and its inverse exist. If the inverse
##hasn't been computed, then its defult value is NULL.

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


## This function first checks whether the inverse of a matrix has been 
##computed. If so, it displays it. If not, it computes the inverse and
##saves it into the special matrix.

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
