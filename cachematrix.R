
##This function creates a special "matrix" object that can cache its inverse
# to caching the inverse of a matrix rather than compute it repeatedly. The

# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to

# 1. set the value of the matrix

# 2. get the value of the matrix

# 3. set the value of inverse of the matrix

# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

## Steps for run:

## > x = rbind(c(1, -2), c(-3, 4))

## > m = makeCacheMatrix(x)

## No cache in the first run

## > cacheSolve(m)

## Retrieving from the cache in the second run

## > cacheSolve(m)

## getting cached data.

 