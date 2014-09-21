## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function must be called and asigned to the variable
# that will be submitted to cacheSolve
# it receives a matrix( a computed inverse matrix)
# and returns the "structure" that will enable cache to a new matrix type

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverseMatrix<-function(solve) m<<- solve
  getInverseMatrix<-function() m
  list(set=set, get=get,
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function
# receives a "makeCacheMatrix" matrix, looks if there si a value in the cache via
# x$getInverseMatrix() if thhis returns NULL computes the inverse of the matrix 
#associated to this received matrix and set the cache with that value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverseMatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setInverseMatrix(m)
  m
}
