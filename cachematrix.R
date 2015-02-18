## the purpose of these functions is to create a special type of matrix object with
## extra functionality such as caching the values passed to it + inverted value when calculated
## and return these values without re-calculating when required

## Creates a special matrix object that can cache it's inverse
## First it asserts that the parameter is of the actual matrix class
## set then sets the internal variable x to the matrix
## get_inverted returns the inverted value if available
## set_inverted sets the internal mtrx value to the given inverted x
## get returns the original matrix
makeCacheMatrix <- function(x = matrix()) {
    mtrx <-NULL
    if(!(class(x) == "matrix"))
    {
      stop("x must be a matrix")
    }
    set <- function(y) {
      x <<-y
      mtrx <<-NULL
    }
    get <- function() x
    set_inverted <-function(inverted) mtrx <<- inverted
    get_inverted <- function() mtrx
    list(set=set,get=get,get_inverted=get_inverted, set_inverted = set_inverted)
}


## function takes the cached matrix object 
## checks if the inverted matrix has already been calculated
## if so, returns it, otherwise it calculates the inverted matrix
## stores it and then
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  invert_m = x$get_inverted()
  if(!is.null(invert_m)){
    message("getting inverted matrix")
    return(invert_m)
  }
  temp_matrix = x$get()
  inv_matrix = solve(temp_matrix)
  x$set_inverted(inv_matrix)
  inv_matrix
}
