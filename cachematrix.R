## developer: Mike Cilliers
## date: 19 Aug 2014
## version: 1.0
## Code forked from rdpeng/ProgrammingAssignment2 at Github.com
## Coursera R Programming Assignment 2

## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly. 
## This R file consists of a pair of functions that cache the inverse of a matrix
## Assume that the matrix supplied is always invertible


## Example of calling the functions below
## A <- matrix(c(2, 4, 3, 1, 5, 7, 3, 5, 8), nrow=3, ncol=3)
## M1 <- makeCachematrix(A)     -- This creates the matrix object M1
## M1$get()  					-- This returns matrix A
## M1$getsolve()				-- This returns NULL
## cacheSolve(M1)				-- This returns the inverse of the matrix A or matrix object M1
## M1$getsolve()				-- This returns the inverse of the matrix A or matrix object M1



## This function, makeCachematrix, creates a special "matrix" object that can cache its inverse
makeCachematrix <- function(x = matrix()) {
	
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
		## retrieve the inverse of the matrix from the cache, return the inverse if it exists (i.e. not null)
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
		## retrieve the matrix
        data <- x$get()
		## calculate the inverse of the matric and assign it to the cache
        s <- solve(data, ...)
        x$setsolve(s)
        s
}