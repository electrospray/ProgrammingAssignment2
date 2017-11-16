## The two following functions combined to produce the inverse of an input matrix.
## Instead of calculating the inverse each time, it is first checked to see if 
## an inversion of the same matrix has been performed. If so, the cached, inversed matrix
## is returned; otherwise inversion is calcuated.

## the makeCacheMatrix function takes in the arguement, and create a special "vector",
## a list with function to set, get, setinverse and getinverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inversed) im <<- inversed
  getinverse <- function() im
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve does the inversion calcuation of the special "vector" created above.
## It first checks to see if an inversion of the same matrix has been performed, and
## returns the cached matrix if that is the case. Otherwise, it performs the matrix
## inversion and return the inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)){
    message("getting cached inversed matrix")
    return(im)
  }

  data <- x$get()
  im <- solve(data)
  x$setinverse(im)
  im
}
