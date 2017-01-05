
## Assignment: Caching the Inverse of a Matrixless 
##
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly.
## 
## a) The pair of functions demonstrates the creation of matrix and 
##    retrival of the inverse matrix from cache if the matrix is not changed. 
##
## b) The <<- operator is introduced that is used to assign a value to an object 
##    in an environment that is different from the current environment
##

##
## makeCacheMatrix -- The function creates matrix object 
##
makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##
## cacheSolve -- The function computes the inverse of the matrix created by makeCacheMatrix funtion. 
##               and retrieves the inverse matrix from the cache if the matrix is not changed. 
##               Retrieval from the cache increases the performance 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInverse()

        if (!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }

        mat <- x$get()

        invMat <- solve(mat, ...)

        x$setInverse(invMat)

        invMat 
}
