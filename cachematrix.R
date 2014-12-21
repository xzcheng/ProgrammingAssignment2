####################################################
# R Programming Programming HW 2, Dec 20 2014
####################################################
# Assignment Requirement:
# Write the following functions:
# (1) makeCacheMatrix: This function creates a special "matrix" 
# object that can cache its inverse.
# (2) cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

## An overall description: the following functions first input a matrix object that can cache its inverse, 
## and then calculates the inverse of this matrix. If the inverse has already been calculated before, then
## cacheSolve will retrive the inverse from the cache.


## This function first input a certain matrix (x), and then calculates its inverse matrix and stores it as s.
makeCacheMatrix<-function(x=matrix()){     # input x will be a matrix
  s<-NULL    # s will be our 'inverse' and it's reset to NULL every
  # time makeCacheMatrix is called 
  set<-function(y){    # takes an input matrix
    x<<-y              # saves the input matrix
    s<<-NULL           # resets the inverse to NULL, basically what happens when a new object is generated.
  }  
  get <-function()x  # this function returns the value of the original matrix
  setsolve<-function(solve)s<<-solve
  getsolve<-function()s  # this will return the cached value to cahceSolve() on subsequent accesses
  list(set=set,get=get,  # this is accessed each time makeCacheMatrix() is called,
       setsolve=setsolve,  # that is, each time we make a new object. This is a list of 
       getsolve=getsolve)  # the internal functions ('methods') so a calling function 
  # knows how to access those methods.
}


## This function either gives the already computed the inverse matrix (s) for the certain matrix inputted by 
## makeCacheMatrix (x) or calculates and then gives the inverse matrix (s) for the certain matrix inputted
## by makeCacheMatrix (x)

cacheSolve<-function(x,...){     # the input x is an object created by makeCacheMatrix
  s<-x$getsolve()                # accesses the object 'x' and gets the value of the solve
  if(!is.null(s)){              # if solve was already cached (not NULL) ...
        message("getting cached data")   # ... send this message to the console
    return(s)                        # ... and return the inverse
     }
  data<-x$get()                # we reach this code only if x$getsolve() returned NULL
  s<-solve(data,...)            # if s was NULL then we have to calculate the inverse
  x$setsolve(s)                 # store the calculated inverse in x 
  s                           # return the inverse to the code that called this fcn  
}


