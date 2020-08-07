##Different actions are labeled in comments explaining their purpose.
## At the bottom is a flow chart adumbrating the process.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    ## 1.1: Clears object m.
  
  set <- function(y){            ## 1.2: Defines x as the input, w/ m again locally being null.
    x <<- y
    m <<- NULL
  }
  
  get <- function() x            ## 1.3: Sets x$get() as the matrix x.
  setsolve <- function(solution) m <<- solution  ## 2.4: defines m within the function here, and only here (I think in function 1), as the solution
  getsolve <- function() m       ## 1.4: Recalls data *after* it has been solved, from 2.4
  list(set = set, get = get,     ## 1.5: Creates accessible list, which we'll call xn, as output
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  m <- x$getsolve()              ## 2.1: Tests if xn$setsolve() is yet defined, and therefore solved. 
  if(!is.null(m)) {
    message("getting cached data") ## 3.1: Returns cached data, and informs you that it is cached
    return(m)
  }
  
  data <- x$get()                ## 2.2: Withdraws data and solves it.
  m <- solve(data, ...)
  x$setsolve(m)                  ## 2.3: Stores data to 2.4
  m                              ## 2.5: Returned Value
}

'''
###############################################################################
Function 1.  Creating a list

Defining m as NULL if not already cached, and x as a matrix retrivalble by xn$get() 
1.1 -> 1.2 -> 1.3 -> 1.5

2.4 -> 1.4, since 1.4 is defined by 1.4, and we have yet to run function 2

###############################################################################
Function 2:  Caching the inverse

2.1, is a portal to three if m, or 1.5, is already defined

Withdraws the matrix, then solves it and stores it to function 1, and then returns it
2.2 -> 2.3 -> 2.4 -> 1.4, then 2.5

If you reach 2.5 then do not go onto three.

################################################################################
Function 3: Returning the Cached inverse

3.1

END
'''