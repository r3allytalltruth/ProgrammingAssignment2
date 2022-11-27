### R Programming Assignment2: Caching the Inverse of a Matrix
## Hi and HELLO- Comments will show my work
# First I make a simple matrix, called matrix1 with a simple matrix inverse n1
## matrix1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## when called, matrix1
##        [,1]  [,2]
##  [1,]  0.50 -1.00
##  [2,] -0.25  0.75
## matrix2 <- makeCacheMatrix(matrix1)
## cacheSolve(matrix2)

# PT1 `makeCacheMatrix`function creates a special "matrix" object, can invert,
# then call > makeCacheMatrix(matrix1)

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of the matrix
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get and set for the matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  ## make list of functions for the matrix
  list(set = set, get = get,
       getinv = getinv,
       setinv = setinv)

}



## PT2 `cacheSolve`computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
## matrix2 <- makeCacheMatrix(matrix1)


cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    ## makes cache matrix (if already computed)
    if (!is.null(inv)) {
      message("inverse is cached")
      return(inv)
    }
    #computes the inverse of the matrix
    m <- x$get()
    inv <- solve(m, ...)
    
    #cache inverse
    x$setinv(inv)
    
    #returns that inverse of the matrix
    return(inv)
}



