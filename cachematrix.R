## A collection of functions designed to take a squared matrix capable
## to get inversed and calculate the inverse if we haven't been calculate it before
## and store it after done to avoid the repeat of calculation when we need it

## makeCachMatrix take a matrix to store and return the summary we need into a list
## including the matrix and its inverse if available or a decleration if it's not
## with special funcctions to deal with data provided.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        inv <<- NULL
        x <<- y
    }
    get <- function() x
    setinv <- function(inver) inv <<- inver
    getinv <- function() inv
    list(set=set,get=get,getinv=getinv,setinv=setinv)

}


## cacheSolve check if the special matrix we made have its inversed 
## calculated to return it( with a decleration message) or calcultes it and return the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
