##This assignment is to write a pair of functions that cache the inverse of a matrix.
##2 Functions required are as below:
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##For this assignment, assume that the matrix supplied is always invertible


## Write a short comment describing this function
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##For this assignment, assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


##These functions above had been tested with the results below:
##> x<-matrix(1:4,2)
##> x
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> b = makeCacheMatrix(x)
##Error: could not find function "makeCacheMatrix"
##> source('C:/Users/SypherQuest/Desktop/Coursera Data Science/ProgrammingAssignment2/cachematrix.R')
##> b = makeCacheMatrix(x)
##> b$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(b)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(b)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##>