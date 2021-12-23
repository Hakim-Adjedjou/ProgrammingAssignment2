## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##create special "matrix" that store it inverse

makeCacheMatrix <- function(x = matrix()) {

   inv<-NULL
  
  ##setting value for the matrix
  set<- function(y) {
    x<<-y
    inv<<-NULL
  }
  ##get the value of the matrix
  get<-function()x
  
  ##setting the inverse of the matrix
  setinverse<-function(inverse) inv<<-inverse
  
  ##getting the inverse of the matrix
  getinverse<-function() inv
  list(set=set, get=get,setinverse=setinverse , getinverse=getinverse)
}


## Write a short comment describing this function

#function that looks for the inverse n retrieve it from cache if possible 
## otherwise calculating it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a<-x$getinverse()
  if(!is.null(a)) {
    print("returning cache data")
    return(a)
  }
  else {
    print("calculating the inverse now")
    b<-solve(x$get())
    x$setinverse(b)
    return(x$getinverse())
  }
}
