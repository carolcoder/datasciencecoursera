makeCacheMatrix <- function(values, nrow, ncol) {
  inverseMatrix <- NULL
  coreMatrix <- a <- matrix(values,nrow=nrow,ncol=ncol)
  set <- function(values2, nrow2, ncol2) {
    
    values <<- values2
    nrow <<- nrow2
    ncol <<- ncol2
    inverseMatrix <<- NULL
  }  
  get <- function() coreMatrix
  getInverse <- function() inverseMatrix
  setInverse <- function(inverted) inverseMatrix <<- inverted
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)

}

cacheSolve <- function(m) {
  inverseMatrix <- m$getInverse()
  
  if (!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  coreMatrix <- m$get()
  inverseMatrix <- solve(coreMatrix)
  m$setInverse(inverseMatrix)
  inverseMatrix
}

mc <- makeCacheMatrix(rnorm(16), 4, 4)
im <- cacheSolve(mc)

