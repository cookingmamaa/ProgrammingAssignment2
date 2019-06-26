## The first function will take our original inversible matrix
## and output a list of functions that can be called upon
## to either determine whether we need to calculate the inverse
## or whether it has been calculated and can be recalled through
## the cache

## makeCacheMatrix makes our list of functions in conjunction 
## with the supplied matrix, the functions are:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function, cacheSolve, will call upon the result of the
## first function, makeCacheMatrix, and will determine whether
## we can simply retrieve the inverse matrix from the cache
## indicating that we have computed it before, or if we have
## not computed it before, it will then compute it and return it
## via the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

#Testing out functions with basic matrix
test_matrix <- matrix(1:4, nrow = 2, ncol = 2) #Basic matrix creation
test_make <- makeCacheMatrix(test_matrix) #Create list w/ functions
test_make$get() #Call matrix w/ get() function to verify creation

#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4

cacheSolve(test_make) #No cache at this point
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve(test_make) #Result retrieved from cache indicated by message
# getting cached matrix
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
