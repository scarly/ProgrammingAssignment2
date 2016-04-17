#These functions try to save in the cache memory and inverse a matrix.

makeCacheMatrix <- function(x = matrix()) {
  local_matrix <- NULL                                         ## set to null
  set <- function(y) {                                    ## Create a function to store the matrix in cache
    cache_x <<- y                                   
    cache_m <<- NULL                                
  }
  get <- function() cache_x                               ## this function return the matrix in cache
  set_cache_m <- function(local_matrix) cache_m <<- local_matrix    ## this function set the value of cache_m in cache   
  get_cache_m <- function() cache_m                       ## this function retrieve value of cache_m 
  list(set = set, get = get,
       set_cache_m = set_cache_m,
       get_cache_m = get_cache_m)
}

cacheSolve <- function(x) {                    
  local_matrix<- x$get_cache_m()      ## this part get the value for m in the cache environment 
  if(!is.null(local_matrix)) {      ## verify if m is NULL.  
    message("cached data ready!!!")  
    return(local_matrix)
  }                                       
  startingmatrix <- x$get()                                      
  endingmatrix <- solve(startingmatrix)   # time to use solve() to invert the matrix. 
  x$set_cache_m(endingmatrix)             
  endingmatrix    ## finally!
}