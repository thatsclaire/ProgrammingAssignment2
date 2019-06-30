## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse property 
  m <- NULL
  #method the set the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #method the get the matrix
  get <- function() x
  #method to set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  #method to get the inverse of the matrix
  getinverse <- function() {
    #return the inverse property
    m
  }
  #return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #just return the inverse if its already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #get the matrix from our object
  data <- x$get()
  #calculate the inverse using matrix multiplication
  m <- solve(data, ...)
  #set the inverse to the object
  x$setinverse(m)
  #return the matrix
  m
}

