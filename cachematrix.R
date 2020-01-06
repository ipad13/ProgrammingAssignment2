##The following two functions will calculate the inverse matrix if the
##matrix has not been calculated before, or they will return the cached
##result if it already calculated the inverse matrix before.

## This is a function that creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL ##set value s to NULL
	set <- function(y){
		x <<- y ##pass the value y to x
		s <<- NULL ##set value s to NULL
	}
	get <- function() x ##get function
	setinverse <- function(solve) s <<- solve ##set the inverse matrix
	getinverse <- function() s ##get the inverse matrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	s <- x$getinverse() ##calls getinverse function
	if(!is.null(s)){ ##if geinverse() returned something
		message("getting cached data") ##print out the message
		return(s) ##return the value
	}
	data <- x$get() ##get the matrix
	s <- solve(data, ...) ##solve the inverse matrix
	x$setinverse(s) ##set the matrix
	s
}
