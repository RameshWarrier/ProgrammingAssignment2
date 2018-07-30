## This experiment is to write a pair of functions, namely, 

## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can 

## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## ---------------Checking the program------------------------
m <- matrix(rnorm(25),5,5)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

# [,1]         [,2]       [,3]        [,4]       [,5]
# [1,]  0.48330078  0.009806606  0.7024519  0.16560616  0.1135345
# [2,]  0.29933929 -0.423894831  2.1512949  0.48080707 -0.5337365
# [3,] -0.41170822  0.491089628 -1.3097331 -0.69570112  0.5412348
# [4,]  0.59929972 -0.345162438  0.8765521 -0.03068036 -0.3339968
# [5,] -0.05048339 -0.613439180  0.2314054  0.11843899 -0.1346903
