## pair of functions that cache the inverse of a matrix.

## The make cacheMatrix creates a matrix object that contains initial matrix,
##and caches it's inverse matrix if it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(inv) m<<-inv
  getinv<-function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## The cacheSolve function computes the inverse matrix, but first, it checkes
##whether the inverse matrix is stored in a "matrix" returned by a makecacheMatrix,
##if it is, it's just retrieves it from makecacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinv(m)
  m
}
