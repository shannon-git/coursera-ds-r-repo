## Put comments here that give an overall description of what your
## functions do

# These functions are used to cache the inverse of a matrix

## Write a short comment describing this function

# The makeCacheMatrix function is used to create a matrix object that can cache it's inverse by making use of the setinverse and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

# The cacheSolve function takes in the makeCacheMatrix object
# It then solves for the inverse of the matrix if it has not already been solved (cached)
# If it is already cached then it just returns the inverse by obtaining it using x$getinverse()

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

my_matrix <- makeCacheMatrix(matrix(1:4, nrow = 2))
my_matrix$get() # displays the matrix
my_matrix$getinverse() # returns NULL as the inverse has not been calculated yet
cacheSolve(my_matrix) # calculates the inverse
cacheSolve(my_matrix) # already Solved so returns the cached inverse
my_matrix$getinverse() # now returns the inverse instead of null
