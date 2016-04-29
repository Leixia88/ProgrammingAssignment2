## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#create a matrix like object, which return a set of funtions that return the matrix or the reverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
# initialize the x_inverse to be NULL
	x_inverse <- NULL
# set function to provide new matrix
	set <- function( y ){
		x <<- y
		x_inverse <<- NULL
	}
	# get the matrix
	get <- function() x
	#set the inverse matrix
	setinverse <- function( inverse ) x_inverse <<- inverse
	#get the inverse matrix
	getinverse <- function() x_inverse
	# return the list of the functions created.
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x', first try get the inverse from the cached, if the cache didn't exit, then
## calculate it and set it to cache and return the result.

cacheSolve <- function(x, ...) {
                # first get the inverse from cached
		x_inverse <- x$getinverse()
		# test if the inverse from cached is NULL or not.
		if( !is.null( x_inverse ) ){
			message( "getting the cached inverse" )
			return( x_inverse )
		}
		# the cached inverse is NULL, so get he matrix from it.
		xmatrix <- x$get()
		# calculate the inverse
		x_inverse <- solve( xmatrix, ... )
		# set the inverse to cache.
		x$setinverse( x_inverse )
		# return the inverse
		x_inverse
}
# uncomment below lines, and run this file with "Rscript cachematrix.R" in your linux shell, gives a quick test of this script.
#x <- makeCacheMatrix( matrix( 1:4, nrow = 2, ncol = 2 ) )
#x1 <- cacheSolve(x)
#y <- matrix( 1:4, nrow = 2, ncol = 2 )
#y1 <- solve(y)
#if( identical(x1,y1) ){
#	print( "x1 == y1")
#}else{
#	print( "x1 != y1" )
#}
