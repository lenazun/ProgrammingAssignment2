
## This function creates a special matrix object that can catch its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  cachematrix <- NULL
  
  set <- function(mtrx) {
    x <<- mtrx
    cachematrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setmatrix <- function(pmatrix) {
    cachematrix <<- pmatrix
  }
  
  getmatrix <- function() { 
    cachematrix> a <- makeCacheMatrix(matrix(1:4,2))
  }
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

  

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
  
