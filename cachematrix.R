## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL		## set m to null
	set <- function(y) {	
		x <<- y		## put y into alternate-universe x		
		m <<- NULL	## put null into alternate-universe y		
	}
	
	get <- function() x	## get the value of the matrix
	setinverse <- function(solve) m <<- inverse	## set the inverse
	getinverse <- function() m	## get the value of the inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## Computes the inverse of the matrix.
## If inverse has already been calculated then cacheSolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	m <- x$getinverse()
	if(!is.null(m)) {	## if m is not a null value then print the message and return value of m
		message("getting cached data")
		return(m)	
	}
	 data <- x$get()
	 m <- solve(data, ...)
	 x$setinverse(m)
	 m
}
