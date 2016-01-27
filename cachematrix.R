## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(input_mat1 = numeric()) {
        inv <- NULL
        setMatrix <- function(input_mat2 = numeric()) {
		if (!is.matrix(input_mat2)) {
		        print("Not a valid matrix. Please enter a valid matrix.")
			return()
			}
		input_mat1 <<- input_mat2
                inv <<- NULL
        }
        getMatrix  <- function() input_mat1
        setInverse <- function(input_inv) inv <<- input_inv
        getInverse <- function() inv
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(input_mat, ...) {
        inv <- input_mat$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- input_mat$getMatrix()
	if (dim(mat)[1] != dim(mat)[2]) {
		print("Not a square matrix. can not calculte the inverse")
		return()
	  }
        inv <- solve(mat, ...)
        input_mat$setInverse(inv)
        inv
}
