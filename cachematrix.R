## makeCacheMatrix - Sets up functions for caching and caches the matrix
## cacheSolve - Returns the matrix inverse (from a chache if previously evaluated)

makeCacheMatrix <- function(x = numeric()) {
     m <- NULL                # sets value of m to NULL in function so previous results are not used
     set <- function(y) {     # defines the function to set the matrix up
          x <<- y
          m <<- NULL
     }
     get <- function() {x}                   # Defines the function to call the value of the matrix
     setinv <- function(solve) {m <<- solve} # Creates a function to set the value of the inverse in the parent environment
     getinv <- function() {m}                # Creates a function to call the value of the inverse from another environment
     list(set = set, get = get,              # Defines function names outside the function
          setinv = setinv,
          getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
     m <- x$getinv()               # Calls the value of m from makeCacheMatrix function
     if(!is.null(m)) {             # Checks if the value of m has been calculated previously
          message("getting cached data")
          return(m)                # If the value is know it is returned and the function stops here
     }
     data <- x$get()          # If the value has not been calculated the value of the matrix is called from the makeCacheMartix function
     m <- solve(data,...)     # The inverse is calculated and assigned to m
     x$setinv(m)              # The setinv function is called to cache the value of the inverse (m) for next time
     m                        # prints m
}
