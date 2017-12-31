## Functions to cach inverses of matrices

## creates a version of a matrix with a cacheable inverse value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## brings the inverse value for a cacheable matrix

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
     message("getting cache data")
     return(inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}
