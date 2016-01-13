# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inmat<- NULL
  set <- function(y) {
  x <<- y
  inmat<<- NULL
}
get <- function() x
setinv<- function(inverse)
inmat<<- inverse
getinv <- function() inmat
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)

}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinv function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inmat <- x$getinv()
  if(!is.null(inmat)) {
    message("getting cached data")
    return(inmat)
  }
  data <- x$get()
  inmat <- solve(data)
  x$setinv(inmat)
  inmat
        
}


## Sample run:
#x <-rbind(c(1, 2), c(2, 1))
#> inmat = makeCacheMatrix(x)
#> inmat$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    2    1
#> cacheSolve(inmat)
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#> cacheSolve(inmat)
#getting cached data
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333

## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > inmat = makeCacheMatrix(x)
## > inmat$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(inmat)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(inmat)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
