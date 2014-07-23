## As required by the assigment description ,two functions:
## 1. makeCacheMatrix that caches a matrix, and adds four
## functions to it -- and an extra object for storing the inverse of it
## 2. a second function that uses these features to get the inverse of
## the matrix: only calculate it if it hasn't been stored yet

## This function creates an object that stores a matrix (passed as argument)
## and possibly also its inverted matrix
## test <- makeCacheMatrix(matrix(c(1,-5,0,0,1,0,0,0,1),3,3))
## Note: test isn't a matrix itself, thus can't be used in calculations
## directly; printing it will show the functions, not (in the above example)
## the 3x3 matrix

makeCacheMatrix <- function(x = matrix()) {
        # initialising the internal variables:
        # im will hold the inverted matrix (but only after calling setsolve)
        im <- NULL
        
        # when a new matrix is assigned to the object,
        # we need to make sure to remove the inverted matrix
        # example of function call: test$set(matrix(c(1:4), 2, 2))
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        #function to return the stored matrix
        # example of function call: test$get()
        get <- function() x
        
        #function to set the value fo the internal inverted matrix object
        # example of function call: test$setsolve(some_inverted_matrix)
        setsolve <- function(solve) im <<- solve
        
        #function to return the cached/stored inverted matrix
        #example of function call: test$getsolve()
        getsolve <- function() im
        
        # calling this function as in the example above (line 8) will result
        # in an object called 'test'. When calling that object without any
        # of the above four functions, we should return a list with these
        # four functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function is meant as a replacement of the 'solve' function for a
## matrix. Calling it with cacheSolve(test) will return the same output
## as calling solve would do on the underlying matrix, but instead of
## always computing it, it will check if the inverse was already calculated;
## and if so, will return that from memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                #return the value & break the function call
                return(m)
        }
        # only executed if m was NULL
        # get the data...
        data <- x$get()
        # ... calculate the inverted matrix ...
        m <- solve(data, ...)
        # ... and cache it
        x$setsolve(m)
        # and of course, resolve it too
        m
}
