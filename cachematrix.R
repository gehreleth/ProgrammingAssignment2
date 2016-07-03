##' This function adds special thunk object to an invertible square matrix x
##' allowing further usage of the cacheSolve() function described below.                    
##'                                                              
##' @param x an inversible (square, non-degenerate) source matrix                     
##' @return a list serves as thunk object for a subsequent \code{\link{cacheSolve}} call
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##' This function makes advantage of cached inverse matrix value if there is any.
##' If there isn't, it calculates such matrix lazily by using \code{\link{solve}}
##' and stores it in the cache afterwards, thus allowing subsequent calls
##' of this function with the same arguments to be executed in O(1) time.
##'
##' @param x a thunk object prepaired by the preceding \code{\link{makeCacheMatrix()}} call
##' @return an inverse matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, diag(nrow=nrow(data), ncol=ncol(data)))
  x$setinverse(inv)
  inv
}
