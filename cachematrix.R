## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 	s <- NULL
  	set <- function(y) {
    	x <<- y
    	s <<- NULL
  	}
	## objects from the parent environment are set up
  	get <- function() x
  	setsolve <- function(solve) s <<- solve
  	getsolve <- function() s
	## the result of the function is a list
  	list(set = set, get = get,
       	setsolve = setsolve,
       	getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	## checks if the inverse matrix exists, if yes, we get the cache data
  	if(!is.null(s)) {
    	message("getting cached data")
    	return(s)
  	}
	## if no, we calculate the inverse matrix
  	data <- x$get()
  	s <- solve(data, ...)
  	x$setsolve(s)
	## the result is inverse matrix
  	s
}
