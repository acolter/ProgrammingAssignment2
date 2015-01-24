## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {	
		x <<- y				
		m <<- NULL			
	}
	
	get <- function() x			## get the value of the matrix
	setinverse <- function(solve) m <<- mean	## set the inverse
	getinverse <- function() m		## get the value of the inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## Computes the inverse of the matrix.
## If inverse has already been calculated then cacheSolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	 data <- x$get()
	 m <- solve(data, ...)
	 x$setinverse(m)
	 m
}
