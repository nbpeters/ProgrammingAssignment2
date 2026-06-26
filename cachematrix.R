## makeCacheMatrix works by caching an inputted matrix in the setMatrix 
## function as x and then creating a list that contains functions to: assign a 
## new matrix while nullifing any cached inverse (i),
## get the current cached matrix, set an inverse (i) of the cached matrix, 
## and get the assigned inverse if there is one assigned

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y){
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setInverse <- function(solved) i <<- solved
  getInverse <- function() i
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve will take the list created in the above command and
## check if the inverse (represented by i) is NULL. If it is then 
## it will proceed to calculate the inverse and cached that. Otherwise
## it will simply return i, the cached inverse of the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data,...)
  x$setInverse(i)
  i
}
