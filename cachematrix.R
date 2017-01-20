## Programming Assignment 2 (week 3) Create 2 functions. 
## The first function 'makeCacheMatrix' creates the inverse of a matrix and caches it.
## The second function 'cacheSolve' either retrieves the inverse of a matrix from cache, or if there is none,
## calculates and returns an inverse matrix.
## Note: we assume that the input matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL 
        ##initialising object to contain output from function below
        
        set <- function (y){
                x <<- y 
                ## set x to y, "<<-"" refers to search through parent environment to find definition of variable
                m <<- NULL 
                ## forcing the value of 'm' that has just been set to be cleared if there is already a valid inverse in cache. 
                ## CacheSolve will have to calculate anew.
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve 
        ##the inverse is set to m
        getinverse <- function() m
        ##retrieves the inverse
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
        ##list of results is generated for use by functions downstream and is cached. List elements are named so we can use $ instead of [[]], 
}



##cacheSolve creates the inverse of matrix created in makeCacheMatrix if there isn't already one in the cache.
##if there is one in cache, it will return the cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                ##checks if the inverse is already cached
                message("getting cached data")
                return (m)
                ## returns m if it is cached
        }
        ## otherwise it calculates the inverse and returns it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## sample output (the first matrix is calculated, the second is retrieved from cache):
##> m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##>  myMatrix_object <- makeCacheMatrix(m1)
##>  cacheSolve(myMatrix_object)
##[,1] [,2]
##[1,]    6    8
##[2,]    2    4
##> cacheSolve(myMatrix_object)
##getting cached data
##[,1] [,2]
##[1,]    6    8
##[2,]    2    4
 