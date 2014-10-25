## Set matrix functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmtrx <- function(solve) m <<- solve
  getmtrx <- function() m
  list(set = set, get = get,
       setmtrx = setmtrx,
       getmtrx = getmtrx)
}

## Return the inverse matrix of 'x' if in cache - get from it!

cacheSolve <- function(x, ...) {
  m <- x$getmtrx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## return from cache
  }
  ## if no cache - calculate matrix solve
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmtrx(m)
  m	
}

## testing:
## a <- makeCacheMatrix()
## a$set( set a square matrix with U[n,n]!=0 ) 
## cacheSolve(a) - at the first time you'll calc solve(a)
## cacheSolve(a) - next time you'll get it from cache!