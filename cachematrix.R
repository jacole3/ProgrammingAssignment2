## There are two functions here: makeCacheMatrix, and cacheSolve. 
## The first one is designed to create an object that can "cache"
## the inverse of any matrix, while the second one is actually able
## to compute the inverse of said matrix. To summarize, if 
## Y is a matrix, and Z is the inverse of Y, then
## x <- makeCacheMatrix(Y)
## cacheSolve(x) should give Z, as long as Y is invertible.


## As can be seen below, the only argument that this first function takes is
## a matrix (say, "x"), which must be invertible. It defines variables
## SetInverse and GetInverse that will come in handy later. makeCacheMatrix
## on its own does not give the inverse of a matrix, but it does cache
## the said inverse for later.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  SetInverse <- function(inverse) m <<- inverse
  GetInverse <- function() m
  list(set = set, get = get,
       SetInverse = SetInverse,
       GetInverse = GetInverse)
}


## The cacheSolve function does not take just the matrix "x" as an argument,
## but it includes the ... argument as well, which, as we've seen earlier
## in the course, indicates a variable number of arguments 
## that are usually passed on to other functions. In this specific case,
## cacheSolve works best when the input IS the prior function; for example,
## for a matrix b, cacheSolve(makeCacheMatrix(b)) is the inverse of b. Thus,
## the purpose of this function is to return a matrix
## that is the inverse of "x" for any invertible matrix "x".

cacheSolve <- function(x, ...) {
  m <- x$GetInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$SetInverse(m)
  m
}

## The following is merely a test. Further tests would not have to be with
## 3x3 matrices; I picked this one relatively randomly. The prompt did not specify
## how many, if any, tests to include. 

a <- c(1, 0, 3, 2, 1, 2, 1, 1, 1)
b <- matrix(a, byrow = TRUE, nrow = 3)
c <- makeCacheMatrix(b)

## The result of this is
## [,1] [,2] [,3]
# [1,] -0.5  1.5 -1.5
# [2,]  0.0 -1.0  2.0
# [3,]  0.5 -0.5  0.5
## That is, the inverse matrix of our original "b".
