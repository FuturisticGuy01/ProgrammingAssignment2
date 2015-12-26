
## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
 	get <- function() x
        setinverse <- function(solve) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse of the special matrix returned by cacheMatrix

cacheSolve <- function(x, ...) {
   m <- x$getinverse()
        if(!is.null(m)) {                                 ##if inverse already calculated, this is returned
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$getinverse(m)
        m
	}
}
