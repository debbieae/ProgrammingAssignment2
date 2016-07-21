## This function gets the inverse of a matrix.  This resulting matrix is cached
## if the same inverse matrix is called, the cached matrix will be returned 
##with a message that states this is a cached version rather than a live computation

## Using the example of makeVector, I have used a modification
##to make a inverse matrix
##I pass in a matrix x instead of a numeric vector from the example

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##cachesolve is based on cacheMean form the example.
##this will pass info to makecacheMatrix and will 
##return a cached inverse matrix if it is available

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}