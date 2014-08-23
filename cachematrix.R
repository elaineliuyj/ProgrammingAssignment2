## makeCacheMatrix creates a special "matrix" to 
## set the value of the matrix, get the value of the matrix, 
## set the value of the inverse of the matrix, and get the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
          x <<- y
          inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function() inv_x
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
## Sample run on makeCacheMatrix
## > x = rbind(c(1,0),c(0,1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## CacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse matrix has already been calculated,it will look it up in the cache and return the inverse matrix without calculating again.
## If the inverse matrix is not in the cache, it will compute the inverse and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message ('getting cached inverse matrix')
                return(inv_x)
        } else {
        data <- x$get()
        inv_x <- solve(data)
        x$setinverse (inv_x)
        return (inv_x)
}
}
## Sample run on cocheSolve
## > cacheSolve(m)
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1


