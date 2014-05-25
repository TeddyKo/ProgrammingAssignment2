## makeCacheMatrix returns a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	## Initiate the inverse matrix
	invMatrix <- NULL
	## Set the matrix
	set <- function(y){
		x <<- y
		invMatrix <<- NULL
	}
	## Get the matrix
	get <- function() x
	## Set the inverse matrix
	setInvMatrix <- function(inverse) invMatrix <<- inverse
	## Get the inverse matrix
	getInvMatrix <- function() invMatrix
	list(set = set, get = get, 
	     setInvMatrix = setInvMatrix,
	     getInvMatrix = getInvMatrix)
}

## cacheSolve computes the inverse of the matrix.
## If the inverse is already calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInvMatrix()
	if (!is.null(invMatrix)){
		message("getting cached data")
		return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInvMatrix(invMatrix)
    invMatrix
}

## Testing scenario and output
## ----------------------------
# > x <- matrix(1:4,2,2)
# > x
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cx <- makeCacheMatrix(x)
# > cx$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(cx)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cx)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
