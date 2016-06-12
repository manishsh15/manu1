## Caching the Inverse of a Matrix
## This function creates a special "matrix" object that can cache its inverse
## The following two functions are used to cache the inverse of a matrix
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set = function(y) {
x <<- y
inv <<- NULL
}
 get = function() x
 setinverse = function(inverse) inv <<- inverse 
 getinverse = function() inv
 list(set=set, get=get, setinverse=setinverse, getinv=getinverse)
}
## The following function returns the inverse of the matrix
## This function assumes that the matrix is always invertible
cacheSolve <- function(x, ...) {
## return: inverse of the original matrix input to makeCacheMatrix()
inv <- x$getinverse()
if (!is.null(inv)) {
message("getting cached data")
return(inv)
 }
## calculates the inverse 
mat.data = x$get()
inv <- solve(mat.data, ...)
 x$setinverse(inv)
 return(inv)
}
 
## To test functions
matrix <- makeCacheMatrix(matrix(1:6,2,2))
> matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> matrix$getinverse()
NULL
> cacheSolve(matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(matrix)
getting cached data
     [,1] [,2]
     [1,]   -2  1.5
[2,]    1 -0.5
> matrix$getinverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> matrix$set(matrix(c(2, 2, 1, 6),2,2))
> matrix$get()
     [,1] [,2]
[1,]    2    1
[2,]    2    6
> matrix$getinverse()
NULL
> cacheSolve(matrix)
     [,1] [,2]
[1,]  0.6 -0.1
[2,] -0.2  0.2
> cacheSolve(matrix)
getting cached data
     [,1] [,2]
[1,]  0.6 -0.1
[2,] -0.2  0.2
> matrix$getinverse()
     [,1] [,2]
[1,]  0.6 -0.1

