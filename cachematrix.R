## Below there are two functions that are used to create a special object that
## stores a matrix and caches its inverse.

## makeCaheMatrix function creates an object that can hold matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function()
                x
        setinvert <- function(z)
                invert <<- z
        getinvert <- function()
                invert
        
        list (
                set = set,
                get = get,
                setinvert = setinvert,
                getinvert = getinvert
        )
        
}

## cacheSolve function takes the object from makeCacheMatrix as an argument,
## and returns the inverse. The actual computation takes place only if
## it hasn't been calculated earlier.

cacheSolve <- function(x, ...) {
        invert <- x$getinvert()
        if (is.null(invert)) {
                message("inverting matrix")
                m <- x$get()
                invert <- solve(m, ...)
                x$setinvert(invert)
                
        }
        ## Return a matrix that is the inverse of 'x'
        invert
}
