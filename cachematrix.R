## JHU Data Sciencec Course: R Programming Assignment 2: Lexical Scoping
## Functions for caching the inverse of a matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##creates a special "matrix"
        r <- NULL
        set <- function(y) { ##sets the value of the matrix
                x <<- y
                r <<- NULL
        }
        get <- function() x ##gets the value of the matrix
        setinverse <- function(inverse) r <<- inverse ##sets the value of the inverse
        getinverse <- function() r ##gets the value of the inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ##computes the inverse of the special "matrix" returned by makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
        r <- x$getinverse()
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        r <- solve(data,...)
        x$setinverse(r)
        r
}
