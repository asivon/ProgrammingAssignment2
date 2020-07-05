## Two functions cache the inverse of the matrix and solve for the inverse.
## Solving for inverse of matrix a
## inverse = cacheSolve(makeCacheMatrix(a))

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  ## Creates a "matrix" object that can cache its inverse.
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInv<-function(inv) m<<-inv
  getInv<-function() m 
  list(set=set, get=get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInv(m)
  m
}
