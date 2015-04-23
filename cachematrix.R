## makeCacheMatrix(m) stores the matrix m and reports back 
## the cached result of some (undetermined) matrix operation, once
## the cache has been set.
##
## cacheSolve uses the makeCacheMatrix facility to cache
## the result of the "solve" matrix operation, and subsequently 
## to reuse that result (instead of recalculating).
##
## testme is used as a sanity check of the implementation.
##


## Provides a mechanism to cache the result of some matrix operation
## (NB: Although I'm sure there are situations where the "set" function would 
## be useful, we don't use it in this exercise.)
## 
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL # start out with no cache
  # report back the underlying matrix
  get <- function() x
  # set the underlying matrix to some new matrix
  set <- function(xNew) {
    x <<- xNew # set to new matrix
    cache <<- NULL # clear cache
  }
  # set the cache, when the calculated result is available
  setcache <- function(calculatedResult) cache <<- calculatedResult
  # report the cached result of a prior calculation
  getcache <- function() cache
  # list of get/set fns
  list(get = get, 
       set = set,
       setcache = setcache,
       getcache = getcache)
}


## Solve for the inverse of a matrix "xCache", with caching of the 
## result for future use.
## "xCache" must be constructed from makeCacheMatrix(x).
##
cacheSolve <- function(xCache, ...) {
  ## Return a matrix that is the inverse of 
  ## xCache <- makeCacheMatrix(x)
  #
  cache <- xCache$getcache()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  # get the underlying matrix
  x <- xCache$get()
  # perform the "solve" calculation
  calculatedResult <- solve(x, ...)
  # save the result
  xCache$setcache(calculatedResult)
  calculatedResult
}


## Sanity check
testme <- function() {
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  h8 <- hilbert(8)
  print("===== 8x8 Hilbert matrix.")
  print(h8)
  print("===== makeCacheMatrix...")
  h8Cached <- makeCacheMatrix(h8)
  print("===== Run cacheSolve 1st time...")
  sh8 <- cacheSolve(h8Cached)
  print(sh8)
  print("===== Check inversion...")
  print(round(sh8 %*% h8, 3))
  print("===== Run cacheSolve 2nd time...")
  sh8_2 <- cacheSolve(h8Cached)
  print(sh8_2)
  print("===== Check inversion...")
  round(sh8_2 %*% h8, 3)
  
}
