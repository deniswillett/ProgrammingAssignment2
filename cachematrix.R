#========================================================#
###            Programming Assignment Two               ###
#========================================================#

# Assignment for the R Programming Data Scientist Course through Coursera
# Create a pair of functions that cache the inverse of a matrix

## The first function will create a function list to establish values of the 
## matrix and its inverse


## makeCacheMatrix will:
## -Set the value of the matrix
## -Get the value of the matrix
## -Set the value of the inverse
## -Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #Initialize m to null
        m <- NULL
        
        #The set function will cache the input value to the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #Return the set value of the matrix
        get <- function() x
        
        #Set m to the the inverse of the matrix
        setinv <- function(solve) m <<- solve
        
        #get the inverse of the matrix
        getinv <- function() m
        
        #Return list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function will calculate the inverse of the matrix if it has not already
## been cached.  

cacheSolve <- function(x, ...) {
        # First check to see if inverse is cached
        m <- x$getinv()
        
        # If inverse is cached return value from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # If the inverse is not cached, get the matrix using the get function
        data <- x$get()
        
        # Calculate inverse
        m <- solve(data, ...)
        
        #Set the inverse
        x$setinv(m)
        
        #Return the inverse
        m
}

#-------------------------------------------------------------------#
##         Now lets check to make sure the functions work.  
#-------------------------------------------------------------------#

        # First, lets run the first function
        mCM=makeCacheMatrix()

        # Next lets set the matrix
        mCM$set(matrix(runif(16), ncol=4))

        # Check to make sure the setting worked
        mCM$get()

        # Calculate inverse
        cacheSolve(mCM)

        # Calculate inverse again - should return 'getting cached data message'
        cacheSolve(mCM)

        # Check to make sure the inverse is really the inverse
        round(mCM$get() %*% cacheSolve(mCM), 2)

########## AWESOMENESSS!!!! ####################

# Thanks for reviewing this.  Enjoy! ...

