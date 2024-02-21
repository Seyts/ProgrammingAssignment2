makeCacheMatrix <- function(matrixInput) {
  # Store the matrix in a private variable
  data <- matrixInput
  
  inverse <- NULL
  
  # Function to set the matrix
  setMatrix <- function(matrix) {
    data <<- matrix
    inverse <<- NULL  # Invalidate the cache when matrix changes
  }
  
  # Function to get the matrix
  getMatrix <- function() {
    data
  }
  
  # Function to compute and cache the inverse
  cacheInverse <- function() {
    if(is.null(inverse)) {
      inverse <<- solve(data)
    }
    inverse
  }
  
  # Return a list of functions
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       cacheInverse = cacheInverse)
}

# Function to compute the inverse of a matrix, caching the result
cacheSolve <- function(matrixCache) {
  inverse <- matrixCache$cacheInverse()
  inverse
}

# Example usage:
# Create a test matrix
test_matrix <- matrix(c(4, 1, 2, 3), nrow = 2)

# Create a cache matrix object
cached_matrix <- makeCacheMatrix(test_matrix)

# Compute the inverse using cacheSolve
cacheSolve(cached_matrix)
