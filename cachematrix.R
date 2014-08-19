## developer: Mike Cilliers
## date: 19 Aug 2014
## version: 1
## Coursera R Programming Assignment 2


## Example of calling the functions below
## A <- matrix(c(2, 4, 3, 1, 5, 7, 3, 5, 8), nrow=3, ncol=3)
## M1 <- makeCachematrix(A)     -- This creates the matrix object M1
## M1$get()  					-- This returns matrix A
## M1$getsolve()				-- This returns NULL
## cacheSolve(M1)				-- This returns the inverse of the matrix A or matrix object M1
## M1$getsolve()				-- This returns the inverse of the matrix A or matrix object M1



## The following function, makeCachematrix, gets called to create a matrix object
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

## The following function, cacheSolve, gets called to return the inverse of matrix object created by makeCachematrix
## This function calls x$getsolve(), in makeCachematrix, to retrieve the cached inverse of the matrix
## If x$getsolve() returns null, it then gets the matrix by calling x$get(), calculates the inverse of the matrix by using the solve() function 
## and then calls x$setsolve to cache the inverse of the matrix
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}