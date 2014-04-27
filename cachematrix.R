## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.Following 
## pair of functions cache the inverse of a matrix. 
## 
## Usage: 
##      To create matrix object that can cache its inverse use
##          cm <- makeCacheMatrix(); cm$set(aMatrix)
##                        OR
##          cm <- makeCacheMatrix(aMatrix)      
##
##      To get inverse of 'aMatrix' use
##          solveMatrix(cm)
##

## makeCacheMatrix creates a special "matrix", which is a list containing a function to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes inverse of the special "matrix" created using makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, it computes inverse
## of the matrix and sets the value of the inverse in the cache.

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
