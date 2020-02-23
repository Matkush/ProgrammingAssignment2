## Put comments here that give an overall description of what your
## functions do
## The functions makeCacheMatrix and cacheSolve 
## allow the user to cache the inverse of a given
## matrix.

## Write a short comment describing this function
## The function makeCacheMatrix starts by setting the 
## values of a matrix, and then retrieving them.
## The same two steps are done with the inverse of 
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
	matriceinversee <- NULL
	defvaleurs <- function(matrice) {
		x <<- matrice
		matriceinversee <<- NULL
	}
	get <- function() x
	defininversee <- function(inversee) matriceinversee <<- inversee
	getinversee <- function() matriceinversee
	list(
		defvaleurs = defvaleurs,
		get = get,
		definversee = definversee,
		getinversee = getinversee
		)
}


## Write a short comment describing this function
## The inverse of a matrix is obtained as a result 
## of the function cacheSolve. First, it checks if
## the inverse was already obtained, and in that case
## returns it. If not, it is calculated via the function
## definversee, then returned.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	matrice <- x$getinversee()
	if (!is.null(matrice)) {
		message("Données:")
		return(matrice)
	}
	donnees <- x$get()
	matrice <- solve(donnees, ...)
	x$definversee(matrice)
	matrice
}
