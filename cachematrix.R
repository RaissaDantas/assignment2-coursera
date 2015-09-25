##This function creates a special "matrix" object that can cache its inverse
## when you call this function say: 
## x = makeCaheMatrix(mat)
## where mat is your input matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() 
    x
  setinverse <- function(solve) 
    m <<- solve
  getinverse <- function() 
    m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and 
##the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

##For this assignment, assume that the matrix supplied is always invertible

##when you call this function you have to call the output of the last function, 
## that was assigned by x

cacheSolve <- function(x, ...) {
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

## example:
## mat = matrix(c(1,4,7,5,1,6,1,4,3),nrow=3,ncol=3)
## x = makeCacheMatrix (mat)
## cacheSolve(x)
