## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function helps to set the matrix,get the matrix,set inverse of matrix and get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<-y
    inverse<<-NULL
  }
  get<-function()x
  setinv<-function(inv)  inverse<<-inv
  getinv<-function()inverse
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## Write a short comment describing this function
##Function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##Inverse has already been calculated (and the matrix has not changed), then cacheSolve  retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
  inverse<-x$getinv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
  
  ## Return a matrix that is the inverse of 'x'
}

##Comment :Resource used to complete assignment : Forum Discussion and other github repositories.
##########################
