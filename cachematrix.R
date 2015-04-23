## makeCacheMatrix: stores a matrix and, after the first run, 
## reports back the cached result of a particular matrix 
## operation done on that matrix.
##
## cacheSolve uses the makeCacheMatrix facility to cache
## the result of the "solve" matrix operation, and subsequently 
## to reuse that cached result (instead of recalculating).
##
## testme is used as a sanity check of the implementation.
##


## Provides a mechanism to cache the result of some matrix operation.
## After the calculation has been done once, the result should be saved
## into the cache using the $setcache fn. The fn $getcache will return
## this result on subsequent calls.
##
## (NB: Although I'm sure there are situations where the "set" function
## would be useful, we don't use it in this exercise.)
## 
makeCacheMatrix <- function( x = matrix() ) {
  cache <- NULL # start out with no cache
  
  # fn to report back the underlying matrix
  get <- function() x
  
  # fn to set the underlying matrix to some new matrix
  set <- function( xNew ) {
    x <<- xNew # set to new matrix
    cache <<- NULL # also clear cache
  }
  
  # fn to set the cache, when the calculated result is available
  setcache <- function( calculatedResult ) cache <<- calculatedResult
  
  # fn to report the cached result of a prior calculation
  getcache <- function() cache
  
  # return the list of get/set fns
  list( get = get, 
        set = set,
        setcache = setcache,
        getcache = getcache )
}


## Solve for the inverse of a matrix "xCache". The result from
## the initial calculation will be cached for future use.
##
## "xCache" must be created with makeCacheMatrix(x).
##
cacheSolve <- function( xCache, ... ) {
  ## Return a matrix that is the inverse of 
  ## xCache <- makeCacheMatrix(x)
  #
  # Attempt to get the cached result
  cache <- xCache$getcache()
  if( !is.null( cache ) ) {
    message("getting cached data")
    return( cache )
  }
  # The cache is empty, so do the work...
  # First, get the underlying matrix
  x <- xCache$get()
  # Then, perform the "solve" calculation
  calculatedResult <- solve( x, ... )
  # Finally, save the result into the cache.
  xCache$setcache( calculatedResult )
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
