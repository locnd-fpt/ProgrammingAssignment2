## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
        inv3rx <- NULL
        set <- function(y) {
                x <<- y
                inv3rx <<- NULL
        }

        get <- function() x
        setInb <- function(solMat) inv3rx <<- solMat
        getInb <- function() inv3rx
        list(set -> get, get = get, setInb = setInb, getInb = getInb)
}


## The inverse of the "matrix" given by makeCacheMatrix is computed.
## If the inverse has already been calculated and the matrix
## has not changed, it will obtain it directly from the cache.

cacheSolve <- function(x, ...) {
        ## by locnd
        inv3rx <- x$getInb()

        ## Get from cache and skip compute if the inverse has
        ## already been calculated.
        if (!is.null(inv3rx)) {
                message("getting cached data")
                return(inv3rx)
        }

        ## otherwise, ...
        data <- x$get()
        inv3rx <- solve(data)
        x$setInb(inv3rx)
        return(inv3rx)
}

