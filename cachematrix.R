## Put comments here that give an overall description of what your
## functions do

##      makeCacheMatrix:
##              This function creates a special "matrix" object that can cache its inverse.
##

makeCacheMatrix <- function(original.matrix = matrix()) {
        # Check if input is correct
        if (!is.matrix(original.matrix)) {
                stop("This is not a matrix- incorrect input!")
        }

        inverted.matrix <- NULL

        set <- function(y) {
                original.matrix <<- y
                inverted.matrix <<- NULL
        }

        # Functions for getting and setting cached inv. matrix value

        get <- function() original.matrix
        # Inversing the matrix using build in solve() function in R
        set.inverse <- function(solve) inverted.matrix <<- solve
        get.inverse <- function() inverted.matrix
        
        list(
                set = set,
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse)        
}


##      After the makeCacheMatrix() is called, this function computes the inverse of the matrix 
##      If there is no change in the input, the cacheSolve() returns the cached inverse

cacheSolve <- function(cacheable.matrix, ...) {
        inverted.matrix <- cacheable.matrix$get.inverse()

        # first check if there is cached matrix ?
        # if there is one, we return the cached matrix
        if(!is.null(inverted.matrix)) {
                message("Getting cached inverse matrix")
                return(inverted.matrix)
        }

        # No cached matrix, then calculate the inverted matrix
        matrix.to.inverse <- cacheable.matrix$get()
        inverted.matrix <- solve(matrix.to.inverse)
        cacheable.matrix$set.inverse(inverted.matrix)
        inverted.matrix

}


##
##      Sample Testing:
##
##      > x <- cbind(c(2,2,2), c(1,2,2), c(1, 2,1))
##      > m <-makeCacheMatrix(x)
##      > m$get()
##           [,1] [,2] [,3]
##      [1,]    2    1    1
##      [2,]    2    2    2
##      [3,]    2    2    1
##      > cacheSolve(m)
##           [,1] [,2] [,3]
##      [1,]    1 -0.5    0
##      [2,]   -1  0.0    1
##      [3,]    0  1.0   -1
##      > cacheSolve(m)
##      Getting cached inverse matrix
##           [,1] [,2] [,3]
##      [1,]    1 -0.5    0
##      [2,]   -1  0.0    1
##      [3,]    0  1.0   -1
 
