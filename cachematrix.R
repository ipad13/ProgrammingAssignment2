##The following two functions will calculate the inverse matrix if the
##matrix has not been calculated before, or they will return the cached
##result if it already calculated the inverse matrix before.

## This is a function that creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y){
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) s <<- solve
	getinverse <- function() s
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	s <- x$getinverse()
	if(!is.null(s)){
			message("getting cached data")
			return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setinverse(s)
	s
}
