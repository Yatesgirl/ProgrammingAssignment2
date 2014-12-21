##Assignment 2 - SRY
##12/17/2014

##The 'makecasheMatrix' and 'cashesolve' functions give the ability to cache potentially time-consuming 
##computations. If the contents of a matrix are not changing, it may make sense to cache the values of 
##the matrix so that when we need it again, it can be looked up in the cache rather than recomputed which
##is what these 2 functions do.


##The funcion 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##setup the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##retreive the value of the matrix
  get <- function() x
  
  ##use the 'solve' function to get the inverse of the matrix
  setmtrx <- function(solve) m <<- solve
  getmtrx <- function() m
  
  ##retrieve the inverse of the matrix
  list(set = set, get = get,
       setmtrx = setmtrx,
       getmtrx = getmtrx)
}



##The function 'cashesolve' returns a matrix that is the inverse of 'x'
cachesolve <- function(x, ...) {
  
  ##Use 'solve' function to get the inverse of the matrix
  m <- x$getmtrx()
  
  ##Check if there is a matrix - print 'getting cashed data' message and the inverse of the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##Else get the inverse of the matrix
  mtrx <- x$get()
  m <- solve(mtrx, ...)
  
  x$setmtrx(m)
  
  ##Print out 'm', the inverse of the matrix
  m
}
