makeCache <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix
  return(m)
}
