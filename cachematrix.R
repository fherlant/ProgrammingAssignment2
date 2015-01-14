# The two functions below aim at computing the inverse of an invertible matrix 
# and putting the result in a cache. Thus when the inverse needs to be found 
# repeatedly, it is already computed and available in a cache, saving run time.
#
# USAGE:
# 1. Create invertible matrix x
# 2. m <- makeCacheMatrix(x) to create special matrix m (ie. cache-enabled x)
# 3. Call cacheSolve(m) to obtain inverse of x
#
# If cacheSolve(m) is called again later, it will return the cached inverse 
# matrix directly.


makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special matrix object that can cache its 
    # inverse. Four methods are attached to this special matrix.
    # 1. set() : Initializes the inverse matrix inv
    # 2. get() : Obtains matrix to be inverted (ie. matrix x)
    # 3. setInverse() : Assigns computed inverse matrix (of x) to inv
    # 4: getInverse() : Obtains cached inverse matrix inv
    #
    # Args:
    #   x: invertible matrix.
    #
    # Returns:
    #   Special matrix that can cache its inverse (matrix + list of functions)
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    # This function computes the inverse of the special matrix returned by
    # makeCacheMatrix. If the inverse has already been calculated 
    # (and the matrix has not changed), then the cacheSolve function retrieves
    # the inverse from the cache.
    #
    # Args:
    #   x: invertible matrix.
    #
    # Returns:
    #   The inverse of matrix x.
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
