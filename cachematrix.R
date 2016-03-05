## This function solves the inverse of a given matrix that is inversible, and to save computing
## capacity, it caches already-calculated values and returns them if appropriate. Godspeed

#actual assignment is to write function that caches inverse of a matrix
#function 1 - makeCacheMatrix, which creates a special matrix using <<- to cache inverse
#function 2 - cacheSolve - computes actual inverse of a matrix returned by function above
# compute actual inverse using the solve function
## solve(X) returns the inverse of the matrix X


# the following two functions are the samples given in the assignment and are used as a template

makeVector <- function(x=numeric()){
     m<-NULL
     set<-function(y){
          x<<- y
          m<<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}

cachemean <- function(x,...){
     m <- x$getmean()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data,...)
     x$setmean(m)
     m
}


## ACTUAL ASSIGNMENT FUNCTIONS

## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     n <- NULL
     set <-function(y){
          x <<- y
          n <<- NULL
     }
     get <- function() x
     setInverse <- function(inverseIt) n <<- inverseIt
     getInverse <- function() n
     list(set = set, get = get,
          setInverse= setInverse,
          getInverse = getInverse)
}

##The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     n <-x$getInverse()
     if(!is.null(n)){
          message("pulling up the cached data")
          return(n)
     }
     data <- x$get()
     n <- solve(data)
     x$setInverse(n)
     n
}
