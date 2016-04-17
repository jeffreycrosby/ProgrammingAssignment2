## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(	set = set,
		get = get, 
		setInverse = setInverse,
		getInverse = getInverse)
}

## This function computes the inverse of the matrix object created
## above, if the inverse had not already been calculated or if matrix
## has changed.  Otherwise, it will return the inverse from the cache.  

cacheSolve <- function(x,...){
	## return a matrix that is the inverse of matrix x
	inv <- x$getInverse()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat,...)
	x$setInverse(inv)
	inv
}
