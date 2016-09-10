# The first function creates a special "matrix" object that can cache its inverse. 
# It allows access to any objects 
# defined in the environment of the original function by creating objects of type list (),
# specifically "getters" and "setters". 
# The "getters" and "setters" retrieve/set data within an object and can be used by 
# subsequent code to access the values of 

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #inverse matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve()
        getsolve <- function() i
        list(set = set, get = get,
             setinverse = setsolve,
             getinverse = getsolve)
  

}
#######################################################################

# The function checks if the inverse has already been calculated. If so, 
# it gets the inverse from the cache. 
# Otherwise, it calculates the inverse of the data and sets the value of 
# the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
