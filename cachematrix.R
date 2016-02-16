## cachematrix.R allows to compute the inverse of a matrix 
## In order to save compute cost, the result of inverse which have already been computed 
## are cached
## It's done with the two following function makeCacheMatrix and cacheSolve 

## The makeCacheMatrix permits to create a matrix which inverse can be cached
## It takes a square matrix as input
## It creates a list containing four functions : 
## - get : get the matrix
## - set : set the matrix
## - getsolve : get the inversed of the matrix
## - setsolve : set the inversed of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Test the input
  if (!is.matrix(x)) {
    stop("Not a matrix !")
  }
  if (dim(x)[1] != dim(x)[2]) {
    stop("Not a square matrix, ahah !")
  } 

  matrice_inv <- NULL
  set <- function(y) {
    x <<- y
    matrice_inv <<- NULL
  }
  get <- function() x
  setsolve <- function(inv) matrice_inv <<- inv
  getsolve <- function() matrice_inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The cacheSolve function permits to compute the inverse of a square matrix
## If the computation has been already done, the cached computation is returned 
## (bypassing the computation of the inverse)
## It takes a square matrix as input and returns the inversed matrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  matrice_inv <- x$getsolve()
  
  ## Test if is it cached
  if(!is.null(matrice_inv)) {
    message("getting cached data")
    return(matrice_inv)
  }
  
  data <- x$get()
  matrice_inv <- solve(data, ...)
  x$setsolve(matrice_inv)
  matrice_inv
}   
