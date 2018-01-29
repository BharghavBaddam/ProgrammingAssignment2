## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Using the below two functions we are going to cache the result of inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInverse = setInv,
             getInverse = getInv)

}


## Write a short comment describing this function
##This function computes the inverse of the matrix created by 
## makeCacheMatrix. If the inverse has been calculated already and if the 
## matrix has not changed, then it is going to retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv

}
