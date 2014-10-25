## Set matrix functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set new matrix 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## set solve matrix
  setmtrx <- function(solve) m <<- solve
  getmtrx <- function() m
  list(set = set, get = get,
       setmtrx = setmtrx,
       getmtrx = getmtrx)
}

## Return the inverse matrix of 'x' if in cache - take from it!
cacheSolve <- function(x, ...) {
  m <- x$getmtrx()
  ## check if matrix already calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## return matrix 
  }
  ## if no matrix in cache - calculate matrix solve
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