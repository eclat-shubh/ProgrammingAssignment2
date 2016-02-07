## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL # This is where the result of the inversion is stored
	set <- function(y)
	{
		x <<- y
		inv <<- NULL # inv to null
	}
	
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse # This sets the inversed matrix
	getInverse <- function() inv # This returns the inversed matrix
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse() # This is to get inversed matric from object x # It will return NULL if uncalculated as defined earlier
		if (!is.null(inv)) # Logical Condition: If the inversion is available then
		{
			message ("Getting cached data")
			return(inv) # This returns the calculated inversion
		}
		mat <- x$get() # Else, use x$get to get the matrix
		inv <- solve(mat,...) # Computation
		x$setInverse(inv) # Set it to object
		inv # Return the solved result
}
