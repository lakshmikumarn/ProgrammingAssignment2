## Two functions makeCacheMatrix and Cache resolve. From command line a matrix should be created. Invoke makeCacheMatrix function to 
## create the list of functions (along with their environment references). Pass the matrix created and list as arguments to cacheSolve. 
##Repeated invocation of cacheSolve will pull the inverted matrix from cache and when you change the matrix data and the same is 
## invoked, the variables will be initialized, including cache to NULL and solve method will be invoked to find the inverse again 

## Function doesn’t accept any argument. It creates a list of functions and respective environment and returns the list

makeCacheMatrix <- function() {

  umat <- NULL
  matinv <- NULL
  
  ## When invoked resets the variables
  init <- function(m) {
    print("Initialize..")
    umat <<- m
    matinv <<- NULL  
  }
  
  ## Set matrix input and reset the matInv whenever invoked
  setMat <- function(m) {
    # Reset the variables (including matinv) only when any value of the source matrix has changed
    if(!is.null(m) && hasMatrixChanged(m)) {
      matinv <<- NULL
      umat <<- m
    }
    
  }
  
  ## Get the matrix data passed as input
  getMat <- function(){ 
    umat 
  }
  
  ## Set the inverse Matrix
  setMatInv <- function(mi){
    matinv <<- mi
  }
  
  ## Return inverse matrix
  getMatInv <- function() matinv
  
  ## Check if the matrix data changed; if anyNA then result will be NA
  hasMatrixChanged <- function(m) {
    y <- is.matrix(m) && dim(m) == dim(umat) && all(m == umat)
    if(y == FALSE) print("Matrix changed")
    !y
  }
  
  list(init=init, setMat = setMat, getMat = getMat, setMatInv = setMatInv, 
       getMatInv = getMatInv, hasMatrixChanged = hasMatrixChanged)


}


## cacheSolve function will check for needs to initialize the variables and use init function of makeCacheMatrix function and determine 
## to pull the inverse of matrix from cache or recalculate.
## accepts two arguments - mat is the matrix for which inverse to be computed and uml is the list from makeCacheMatrix to invoke its 
## functions
 
 
cacheSolve <- function(mat, uml, …) {

 if(is.null(uml$getMat()) ||  uml$hasMatrixChanged(mat)){
    uml$init(mat)
  }
  
  mi <- uml$getMatInv()
  
  if(!is.null(mi)){
    print("Matrix inverse from cache")
    print(mi)
    return(mi)
  }
  
  mi <- solve(mat)
  uml$setMatInv(mi)
  print(mi)
  mi

}

## Flow of execution in R prompt

# > l <- makeCacheMatrix()
# > mat <- matrix(1:4, nrow=2,ncol = 2)
# > inv <- cacheSolve(mat,l)
# -----invoke again to fetch from cache ----
# > inv <- cacheSolve(mat,l)
# ------ change matrix
# > mat <- matrix(2:5, nrow=2,ncol = 2)
# > inv <- cacheSolve(mat,l)

