## Cache the inverse of a matrix 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL 
    set<-function(y){
        x<<-y
        i<<-NULL  
    }
    get<-function() x 
    setinverse<- function(solve) i<<- solve 
    getinverse<- function() i 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## This function computes the inverse of the special "matrix"; if the inverse has already been cacluated, retrieves inverse from cache 

cacheSolve <- function(x, ...) {
       i<-x$getinverse() 
       if(!is.null(i)){
           message("getting cached data") 
           return(i) 
       }
       data<-x$get() 
       i <- solve(data)
       x$setinverse(i)
       i
}
