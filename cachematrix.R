##The functions below are a solution to assignment from Sl4v0 on demonstrating
##the lexical scoping functionalities in R 

##makeCacheMatrix creates a special "matrix" object that can cache its inverse 
##and return a list of functions as a result that can set/get results of its 
##internal variables (matrix and its inverse)
##

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInv <- function(z) invMatrix <<- z
        getInv <- function() invMatrix
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


##cacheSolve: This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInv()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setInv(invMatrix)
        invMatrix
        
}

