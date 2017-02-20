## The following are a pair of functions that can cache the inverse of a matrix

## Creates special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  t<-NULL
  
  ##the method to set the matrix
  set<-function(matrix){
    x<<-matrix
    t<<-NULL
  }
  
  ##the method to get the matrix
  get<-function(){
    x
  }
  
  ##the method to set the inverse of the matrix
  setInverse<-function(inverse){
    t<<-inverse
  }
  
  ##the method to get the inverse of the matrix
  getInverse<-function(){
    t
  }
  
  ##to return a list of methods 
  list(set=set, get=get,
       setInverse= setInverse,
       getInverse= getInverse)
}  

##Computes inverse of special matrix returned by "makeCacheMatrix" function
##above
##If inverse has already been calculated, cacheSolve should 
##retrieve inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  
  ##returns inverse if it's already set
  if(!is.null(m)){
          message("getting cached data")
          return(m)
  }

  ##gets the matrix
  data<-x$get()
  
  ##To calculate the inverse (using matrix multiplication)
  m<-solve(data) %*% data
  
  ##sets the inverse 
  x$setInverse(m)
 
  ##returns the matrix
  m
}
