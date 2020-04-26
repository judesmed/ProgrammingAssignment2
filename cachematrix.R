##the function create a matrix and invert it 
##it also check if the inverstion had already bee calculated stored in the cache
## if the inv hasn't been done the function does it and store it in the cache
##makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x<<-y
                inv<-NULL
        }
        get <- function(inverse) inv<<-inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)

}


## this function first check in the cache memory if the matrix has already been inversted
##if yes it return the inv stored in cache
## otherwise it operate the inversion and store it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
