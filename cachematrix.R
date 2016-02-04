## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Temp_Inverse <- NULL # reset with NULL
  Set <- function(Matrix_2) {
    Matrix_1 <<- Matrix_2
    Temp_Inverse <<- NULL # reset with NULL
  }
  Get <- function() Matrix_1
  Set_Inverse <- function(Inverse) Temp_Inverse <<- Inverse
  Get_Inverse <- function() Temp_Inverse
  list(Set = Set, Get = Get, Set_Inverse = Set_Inverse, Get_Inverse = Get_Inverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Temp_Inverse <- Matrix_1$Get_Inverse()
  if (!is.null(Temp_Inverse)) {
    message("getting cached data")
    return(Temp_Inverse)
  }
  Matrix_3 <- Matrix_1$Get()
  Temp_Inverse <- solve(Matrix_3, ...)
  Matrix_1$Set_Inverse(Temp_Inverse)
  Temp_Inverse
}

# List Objects
ls()

# Testing 2x2 Matrix
Test_My_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
Test_My_Matrix$Get()

# Inverse Matrix
Test_My_Matrix$Get_Inverse()
# Find in Cache
cacheSolve(Test_My_Matrix)
# Find in Cache
cacheSolve(Test_My_Matrix)
# Get current Matrix
Test_My_Matrix$Get_Inverse()

# Testing 3x3 Matrix
Test_My_Matrix <- makeCacheMatrix(matrix(1:4, 3, 3))
Test_My_Matrix$Get()

# Inverse Matrix
Test_My_Matrix$Get_Inverse()
# Find in Cache
cacheSolve(Test_My_Matrix)
# Find in Cache
cacheSolve(Test_My_Matrix)
# Get current Matrix
Test_My_Matrix$Get_Inverse()
