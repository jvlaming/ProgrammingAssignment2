## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## turn a regular matrix in an object that 
## - initializes inverse value
## - stores original matrix (set)
## - returns original matrix (get)
## - stores inverse value in cache (setinverse)
## - looks up inverse value in cache (getinverse)
makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse 'i' value at NULL
    i <- NULL
    ## 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## get : return the source matrix
    get <- function() x
    ## setinverse: calculate the inverse 'i' using function 'solve'
    setinverse <- function(solve) i <<- solve
    ## getinverse: return the inverse value 'i'
    getinverse <- function() i
    ## create a list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## Function tries to lookup inverse value of matrix object 'x' in cache. 
## If found then this result is returned. 
## If not found then inverse 'i' is calculated using 'solve', stored in 'x' and returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## try to get a value for inverse 'i' from the matrix object 'x'
    i <- x$getinverse()
    ## if the returned value does not equal null then it has been taken from cache 
    ## and returned (end of function)
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## if no value was found in cache then get the source matrix from the matrix object
    data <- x$get()
    ## .. calculate its inverse 'i'
    i <- solve(data, ...)
    ## .. store the inverse 'i' in the matrix object using setinverse
    x$setinverse(i)
    ## and return inverse 'i' as function result 
    i    
}
