## The functions will calculate the inversion of matrix and cache them if they have been calculated

## This function creastes a special "matrix" object that can cashe its inverse


makeCacheMatrix <- function(x = matrix()) {
      Inver <- NULL                                        ##Initialize the "Inverse" variable to NULL
      set<-function(y){                                    ## the numeric argument passed into"makeCashceM" func
        x<<-y                                              ##set "x" for the function environment to "y"
        Inver<<-NULL                                       ##set"Inver" for the makeCacheMatrix" environemtn to NULL
      }
      get<-function() x                                    ##Create a function "get" in the "mkCacheMtx" and assign a matrix to it
      setinverse<-function(solve)Inver<<-solve             ##Takes a value("solve") and sets it to the value of Inver from the 
                                                           ##"mkCacheMtr"frame.
      getinverse<-function() Inver                         ##returns the value of"Inver"
      list(set = set, get = get,                           ##list out the values of the functions in the "mkCacheMtr" frame
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not changed), then cacheSovle
## should retrieve the inverse from the cashe

cacheSolve <- function(x, ...) {                          ##Return a matrix that is the inverse of 'x' 
       Inver<-x$getinverse()                              ##goes to the "x" environment and assign the "Inver" value from that environment to this one
       if(!is.null(Inver)){
         message("getting cached data")                   ##if there is a value to Inver(not NULL), that means it has been calculated before
         return(Inver)                                    ##fetch the calculated result from cache
       }
       data<-x$get()                                      ##If this inversion has not been calculated before,get the x-matrix to store in data
       Inver<-solve(data,...)                             ##Calculate the inverse of the maxtrix "x"
       x$setinverse(Inver)                                ##Assign the calculated inverse to the "x"
       Inver                                             
}
## The notes are written according to a post on the cousera forum by Stan Kolis