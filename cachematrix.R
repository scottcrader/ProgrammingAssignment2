## This function accepts a square matrix as an argument,
## then returns a list of methods that can be passed to cacheSolve()

cacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function can only accept as an argument, an object of type 
## 'cacheMatrix', and returns the inverse of the matrix 

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    
    ## if the inverse has already been calculated, retrieve from cache
    ## and skip the computation
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, compute the inverse, and store in the cache
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv)
    
    return(inv)
}

## unit test and time-check function found online

# set.seed(1110201)
# r = rnorm(1000000)
# mat1 = matrix(r, nrow=1000, ncol=1000)
# test(mat1)
# 
# test = function(mat){
#   ## @mat: an invertible matrix
#   
#   temp = cacheMatrix(mat)
#   
#   start.time = Sys.time()
#   cacheSolve(temp)
#   dur = Sys.time() - start.time
#   print(dur)
#   
#   start.time = Sys.time()
#   cacheSolve(temp)
#   dur = Sys.time() - start.time
#   print(dur)
# }
