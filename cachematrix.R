## This file contains a pair of functions that solves and 
## caches the inverse of a matrix.
## *assumes that the matrix supplied is always invertible

# Usage
## 1. create makeCahce Matrix object
##   > x <- makeCacheMatrix()
## 2. assign matrix to the object
##   > x$setmatrix(matrix(c(1,2,3,4), 2, 2))
## 3. calculate and store the inverse
##   > cachematrix(x)
## 4. retrieve cached inverse matrix
##   > x$getmatrix()



## make Cache Matrix  allows user to set the matrix and retrieve the matrix and also its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}



## cachematrix function checks if the inverse of the matrix has been calculated
## if not it will calculate and store the value in the makeCacheMatrix object.

cachematrix <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix which is inverse of 'x'")
                return(m)
        }
        message("matrix not set, calculating inverse")
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}