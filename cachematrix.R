## These functions can create a mtrix and return its inverse and are able to cache the inverse if
## if its already calculated

## makeCacheMatrix create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set<- function(y){
           x <<- y
           i <<- NULL
        }
        get<- function() x
        setinverse<- function(inverse) i <<- inverse
        getinverse<- function() i
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}

## Function cachesolve returns inverse of the special matrix created by above function. If he inverse 
## has already been calculated and matrix has not changed then it can retrieve its inverse from cache
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<- x$get()
        i<- solve(data,...)
        x$setinverse(i)
        i
}
