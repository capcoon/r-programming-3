# makeCache Function is created to set and get a matrix in Cache

makeCache <- function(x = matrix()) {
  m <- NULL # initialize cache with NULL value
  set <- function(y) {
    x <<- y
    m <<- NULL # create matrix in working environment
  }
  get <- function() x
  setMatrix <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse) # get and set Matrix to solve inverse matrix and stored as list type
}

# cacheSolve Function is created to solve inverse matrix created in makeCache function
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
# determine whether nothing is stored in cache. If matrix is stored in cache, return the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # create matrix if nothing is stored in cache
  data <- x$get()
  m <- solve(data, ...) # return the inverse matrix
  x$setMatrix(m) # set the inverse matrix in cache
  return(m) return # return matrix in Cache
}
