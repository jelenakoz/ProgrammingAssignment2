## functions are geting inverse of an matrix. Inverse is looked up in cache .
## If there is not, it is calculated and cached.

## This function make list of 

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}	
	get<-function()x
	setinv<-function(inve)inv<-inve
	getinv<-function()inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<-x$getinv()
	if(!is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}
	data<-x$get()
	inv<-solve(x)
	x$setinv(inv)
	inv
}
