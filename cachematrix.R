# Creates a 'special' cache matrix. Initialize it with a normal matrix (e.g.,
# m <- makeCacheMatrix(matrix(1:4, 2, 2))) and it returns a list with four
# functions. The four functions can be used as follows: m$get() retrieves the
# matrix, m$set(matrix(...)) sets a new matrix, m$getinverse() retrieves the
# currently cached inverse matrix (which is NULL if it was not solved and set
# before) and m$setinverse(inverse) can be used to cache the solved matrix.

makeCacheMatrix <- function(x = matrix()) {
        # The inverse is not yet calculated when creating the 'special' cache
        # matrix, so initialize it to NULL.
        i <- NULL
        # Define the four functions and return the list containing them.
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Pass a 'special' cache matrix to cacheSolve to either retrieve the inversed
# matrix from its cache or, if there is no cached matrix, calculate the
# inverse, cache it and return it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("retrieved cached data")
                return(i)
        }
        # If the program reaches this point then there was no cached inverse of
        # the matrix available so the inverse needs to be calculated and set.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
