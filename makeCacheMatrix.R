
##Our aim in this experiment is to write a pair of functions, namely,
##"makeCacheMatrix" and "cacheSolve" that caches the inverse of a matrix
##makeCacheMatrix is a function which creates a special "matrix" object that can
##cache its inverse for the input (which is an invertible square matrix)
  
makeCacheMatrix <- function(x = matrix()){   
  
  inv <- NULL                                                     ##initialize inv as NULL;will hold value of matrix inverse 
  set <- function(y){                             
    x <<- y                                                       ##value of matrix in parent environment
    inv <<- NULL                                  
  } 
  get <- function(){x}                                            ##returns value of the matrix argument
  setInverse <- function(inverse){inv <<- inverse}                ##assigns value of inverse in parent environment 
  getInverse <- function(){inv}                                   ##gets the value of inv where called 
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)      
}
  
##cacheSolve is a function which computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache)
  
  
cacheSolve <- function(x,...){                      
  
  inv <- x$getInverse()                                            
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
