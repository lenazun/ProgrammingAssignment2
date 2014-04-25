
## This function creates a special matrix object that can catch its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #set the value of the cache matrix to null
  cachematrix <- NULL
  
  #function to set the value of the matrix
  set <- function(mtrx) {
    x <<- mtrx
    cachematrix <<- NULL
  }
  
  #function to get the matrix data
  get <- function() {
    x
  }
  
  #set the matrix and cache 
  setmatrix <- function(pmatrix) {
    cachematrix <<- pmatrix
  }
  
  #get the value of the matrix
  getmatrix <- function() { 
    cachematrix
  }
  
  #organize everything as a list
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

  

## This function returns the value of the inverse of the matrix

cacheSolve <- function(x, ...) {

#gets the value of the matrix  
  m <- x$getmatrix()
  
#if there is a cache retrieves the value  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

#returns the reverse of the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
  
