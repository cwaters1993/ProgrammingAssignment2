## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        return(list(set=set,get=get,
                    setinverse=setinverse,
                    getinverse=getinverse)) 
}


## The following function calculates the inverse of the
## special "matrix" created with the above function. 
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it
## calculates the inverse of the data and sets the value of
## the inverse in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                return(i)
        }
        matr <- x$get()
        i <- solve(matr, ...)
        x$setinverse(i)
        return(i)
}
