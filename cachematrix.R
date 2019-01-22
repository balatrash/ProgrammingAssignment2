## The below 2 functions provide getters and setter of a matrix 'x'
## and its inverse and caches them for later use
## they follow the same structure and flow of the makeVector and cachemean functions
## covered in teh assignment description

## makeCacheMatrix is function that returns a list of functions
## that give the ability
## to set or get the matrix and
## to get and set the inverse of the matrix
## and store the matrix and its inverse for later use

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL

	## sets the new value of x and resets the inverse to null
	set <- function (y) {
		x <<- y
		i <<- NULL
	}

	## gets the value of x and returns it
	get <- function() x

	## sets the inverse 'i' of the matrix x
	setinverse <- function(inverse) i <<- inverse

	## returns the stored inverse 'i' of the matrix x
	getinverse <- function() i

	## return a list of functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## cacheSolve will check if the inverse of a matrix x is already computed
## using functions from makeCacheMatrix function
## if so it will return the cached value
## else will compute the inverse, cache it and return it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'	
	
	i <- x$getinverse()

	## if the inverse exists print to console and return the inverse (halting the function) 
	if(!is.null(i)) { 
		print("The inverse is already computed. Retrieving it from cache")
		return(i)
	}

	## else ("else" clause not needed because of the "return" function above)
	## compute inverse, cache it and return

		matrix <- x$get()
		i <- solve(matrix,...)
		x$setinverse(i)
		i
	
}
