## These functions use cached values to prevent recomputing 
## the inverse of a matrix unnecessarily, as that calculation is
## time-consuming and memory-intenstive


## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(one_matrix = matrix()) {
	stored_inverse <- NULL                   	# Initialize the 
												# 'stored_inverse' variable 
												# to NULL; must do so that 
												# the function doesn't hang 
												# the first time through.
												
	set <- function(y) {                  		# 'y' in this case is the 
												# numeric arg passed into 
												#'makeCacheMatrix'.	
												
		one_matrix <<- y                     	# Set 'one_matrix' for the 
												# function environment to 'y'.
												
		stored_inverse <<- NULL              	# Set 'stored_inverse' for the 
	}											#'makeCacheMatrix' environment to NULL.
	

	get <- function() {							# Create a function 'get' in 
		return(one_matrix)						# the 'makeCacheMatrix'
	}											# parent and assigns a matrix 
												# to it.  Note that this makes 
												# little sense without the context 
												# of the cacheSolve function.


	setinv <- function(replacement_inverse){	# Takes a value 
		stored_inverse <<- replacement_inverse	# ('replacement_inverse') and 
	}											# sets it to the value of 
												# 'stored_inverse' in the 
												# 'makeCacheMatrix' environment.

	getinv <- function() {						# Returns the value of 
		return(stored_inverse)					# 'stored_inverse' from the
	}											# 'makeCacheMatrix' frame. 
												# Needs the context of the
												# 'cacheSolve' function to 
												# make sense.

	list(set = set, get = get,            		# Lists out the values of the 
		setinv = setinv,						# functions in the
        getinv = getinv)            			# 'makeCacheMatrix' frame. 
}

## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve function retrieves the 
## inverse from the cache.

cacheSolve <- function(two_matrix, ...) {
	local_inverse<- two_matrix$getinv()		# Goes to 'one_matrix'(in 
												# the 'makeCacheMatrix' 
												# environment)and assigns the 
												# 'local_inverse' value from 
												# that environment to this one.

	if(!is.null(local_inverse)) {				# Tests if the 'one_matrix' 
												# has been evaluated before(i.e.
												# stored_inverse is not NULL).
												
		message("getting cached data")			# If there is a cached value, 
		return(local_inverse)					# the function prints the 
	}											# message and the value of
												# local_inverse (same as the 
												# stored_inverse from 
												# makeCacheMatrix).
	
	
	local_matrix <- two_matrix$get()			# If the matrix has never been
												# evaluated before, pull 
												#'one_matrix' into a local 
												# matrix called 'local_matrix'.
												
	local_inverse <- solve(local_matrix, ...)	# Calculate the inverse of the 
												# matrix 'local_matrix' by 
												# calling 'solve' function.
									
	two_matrix$setinv(local_inverse)			# Assign the calculated 
												# inverse to the 'one_matrix'
												# environment using the 
												# 'setinv' function.
												
	return(local_inverse)						# Display the inverse.
}
