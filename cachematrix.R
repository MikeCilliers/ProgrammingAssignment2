## Computing the inverse of a special "matrix" by calling the function cacheSolve
## The special "matrix" must be a n by n matrix in order to compute the inverse
## Computing the inverse of a n by n matrix can be costly to compute 
## This can be addressed by caching the inverse of a special "matrix", by calling the function makeCacheMatrix
## 

## The following function caches the inverse of a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
	solve(x)

}

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
	## Return the inverse matrix of a special, n by n, "matrix"
		
	## Check if the matrix is a n by n matrix firts
	if (nrow(x) == ncol(x)) {
		makeCacheMatrix(x)
	} else {
		print("Unable to determine the inverse of the matrix as it is not a n by n matrix")
	}
}
