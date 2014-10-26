## These functions allow the user to input a matrix and compute its inverse
## To improve performance, the inverse of a matrix is cached. 
## If the input matrix inverse does not exist, it is computed and then cached.
## If the input matrix inverse has already been computed, its value is retrieved from the cache

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


##  cacheSolve: This function computes the inverse the "matrix" returned by makeCacheMatrix.
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}
 
