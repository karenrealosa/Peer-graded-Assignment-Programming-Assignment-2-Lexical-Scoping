##
## I set the input "x" as matrix
## I also set the solved value "s" as null
## I changed every reference to "mean" and to "solve"

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) {s <<- solve}
  getsolve <- function() {s}
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Same function applies, I changed "mean" to "solve" and "m" to "s"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if (!is.null(s)) {
          message("getting cached data")
        return(s)
  }
  mat <- x$get()
  s <- solve(mat, ...)
  x$setsolve(s)
  s
}
