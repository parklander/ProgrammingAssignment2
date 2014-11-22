## This file contains a pair of functions that solves and 
## caches the inverse of a matrix.
## *assumes that the matrix supplied is invertible

# Usage
## 1. create makeCacheMatrix object
##   > x <- makeCacheMatrix()
## 2. assign matrix to the object
##   > x$set(matrix(c(1,2,3,4), 2, 2))
## 3. calculate and store the inverse
##   > cacheSolve(x)
## 4. retrieve cached inverse matrix
##   > x$getInverse()



## makeCacheMatrix sets the matrix (x$set) and retrieves the matrix {x$get()}
## and its inverse (x$getInverse()). Also exposes setInverse allowing cacheSolve
## to store inverse in makeCacheMatrix object.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve checks if the inverse of the matrix has been calculated
## if not it will calculate and store the value in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached matrix which is inverse of 'x'")
                return(m)
        }
        message("matrix not set, calculating inverse")
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}