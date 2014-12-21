EmakeCacheMatri<- function(x = matrix()) {
    m <- NULL
###if user makeVector(),she must define x by run makeVector()$set(y),
##y is the matrix which we need to compute its inverse matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
####if the user run makeVector(x),it define the matrix,so we need to capture the result
    get <- function() x
####the following code can help the user to define the answer by herself
##makeVector$setmean(solve),solve is the inverse matrix
    setsolv <- function(solve) m <<-solve 
####when the user run x=makeVector(matrix);cachemean(x)
####x=makeVector();x$set(vecotor);cachemean(x)
###it will triger the getmean function to capture the result
    getsolv<- function() m
    list(set = set, get = get,
         setsolv = setsolv,
         getsolv = getsolv)
    
} 

cacheSolve <- function(x, ...) {
    m <- x$getsolv()
    if(!is.null(m)) {
        print("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolv(m)
    m
    
}
###example
###A=matrix(c(1,2,1,3),nrow=2)
###x=makeCacheMatri(A);cacheSolve(x);cacheSolve(x);nter file contents here
