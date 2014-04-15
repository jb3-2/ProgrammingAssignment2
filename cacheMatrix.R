## The following functions help creating matrices and creating their inverted matrix.
## Inverted matrixes are cached along the original matrix in order to save on computing time. 


## Creates a matrix with its inverted matrix saved along.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s 
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Calculates inverted matrix and caches it along the original. Returns cache if already present.
cachesolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}



# Testing the functions
t <- matrix(1:4,2,2)
t
t1 <- makeCacheMatrix(t)
# Create inverted matrix to copmpare with resault from custom function
solve(t)
# First invocation, should not hit cache
cacheSolve(t1)
# Second invocation, should hit cache
cacheSolve(t1)
# re-set matrix, cache should be emptied
u <- matrix(5:8,2,2)
t1$set(u)
# First invocation, should not hit cache
cacheSolve(t1)
# Second invocation, should hit cache
cacheSolve(t1)




## Example functions given in course
# 
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
