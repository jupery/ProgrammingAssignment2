## The two function, 'MakeCacheMatrix' and 'cacheSolve' work along with each other to
## return an inverse of a matrix in a memory-saving way.
## These two functions allow you to avoid repeating the calculation of an inverse 
## by firstly checking a cache, returning the inverse if it has already been stored.
## If the inverse is not found, the functions calculate and return the inverse 
## as well as storing it for future reference.
## 
## Both functions assume that given matrix is both square and invertible.
## They will return an error otherwise.



## The function 'makeCacheMatrix'converts a matrix of an atomic type
## to a list that contains the original matrix and a few other attributes
## required to calculate and store its inverse.

makeCacheMatrix <- function (x=matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setInverse <- function (inverse) i <<- inverse
  getInverse <- function() i
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 'cacheSolve' returns an inverse of a matrix by either
## referring to a cache of the matrix, or, if not found, calculating the inverse.


cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}