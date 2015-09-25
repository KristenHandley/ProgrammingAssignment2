## The first function will create a "special" vector which
##sets and gets the value of the vector, then 
##sets and gets the value of the inverse
##The second function will calculate the inverse of the vector created in the first fucntion
##Before this though, it will check to make sure the inverse has not previously been calculated.
##If it has, then it will take this from the cache, otherwise it will calculate the inverse and place it in the cache

## This first function will create a matrix which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    mc <- NULL
    set <-function(y){
      x <<-y
      mc<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) mc<<-inverse
    getinverse<-function()mc
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This second function will comput the inverse of the first function
##But if it has already been calcuated it will retrieve it from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cs<-x$getinverse()
  if(!is.null(mc)){
    message("getting cached data")
    return(mc)
  }
  data<-x$get()
  mc<-solve(data,...)
  x$setinverse(mc)
  mc
}
