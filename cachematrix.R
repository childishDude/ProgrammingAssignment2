## The following functions split their roles, between storing matrix values and
## solving the matrix. The first function takes an input matrix and stores this 
## value and creates a solution variable 'm' which stores cached solutions. The 
## second fuction accesses these values, to check if the cache is empty, and if so 
## solves using the input variable and stores the new solution in the cache, 
## otherwise returns the cached value.

 
 # The following function takes an input matrix as its only argument. The function
 # body stores the input matrix variable and a solution matrix variable, which 
 # serves as a cache to store solution values. It also creates and returns a list 
 # of four child functions, which can access (get) and change these variables (set)
 # for the main parent function through the assignment operator '<<-'.
makeCacheMatrix <- function(x = matrix()) {
  
  # 'm' is the solution matrix variable. It is set to NULL in the beginning, as 
  # there is no inverse matrix calculated yet. It serves as the cached solution.
  m<-NULL
  
  # The Set matrix function resets the input matrix variable 'x' to 'y'. It also 
  # sets 'm' to null, ensuring that once a new input is set, the solution variable 
  # is also reset. Both 'x' and 'm' are reset for the entire main parent 
  # function environment as '<<-' is used to set, so other functions may access
  # the new values
  setMatrix <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # the Get matrix function returns the value of the curret input matrix variable 'x'.
  getMatrix <- function(){
    x
  }
  
  # This function takes the solution as an argument. This solution is set for the 
  # entire parent function envrionment in 'm', the solution matrix variable. This
  # is the cached solution and is only reset when the input is reset, through the 
  # setMatrix function, or through the main parent function, makeCacheMatrix.
  setCacheMatrix <- function(matrix){
    m <<- matrix
  }
  
  # This function returns 'm', which is the solution matrix variable. If a 
  # solution has been cached by setCacheMatrix, this function will return the
  # cached value, else it will return null.
  getCacheMatrix <- function(){
    m
  }
  
  # The main function returns a list of all the constructed functions above, 
  # through which the 4 functions can be accessed and used.
  list(setMatrix=setMatrix,getMatrix=getMatrix,setCacheMatrix=setCacheMatrix,
        getCacheMatrix=getCacheMatrix)
}

# The cachesolve function takes in the list of functions returned by the 
# makeCacheMatrix function and accesses the list of functions to solve the input 
# matrix variable and cache it, or to retrieve a cached solution.
cacheSolve <- function(x, ...) {
  
  # Assigns variable the current cached value
  m <- x$getCacheMatrix()
  
  # if the cached value is not null (has a solved value), then cacheSolve simply 
  # returns that value as the solution.
  if(!is.null(m)){
    message("getting cached data")
    m
  }
  # If the cached value is null (hasn't been solved yet), the solution is computed.
  # Then it is stored as the new cached value using setCacheMatrix.
  compM <- x$getMatrix()
  m <- solve(compM, ...)
  x$setCacheMatrix(m)
  m
}