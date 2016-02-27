## the two functions: makeMatrix and cacheSolve work together to realize 
## the function of compute and cache the inverse of a certain matrix.

## with proper input ,this function will generate 4 functions. which mainly
## tell us what we've loaded into the function that is ,the input and probably the 
## inverse of it if the input has been calculated beforeser.

makeMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## this function cooperate with the upper one, it get in data from the upper function
## and if there has been an inverse, meaning that we've calculated the inverse
## the message "getting cached data" will appear on the window.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
 ## Return a matrix that is the inverse of 'x'
}
