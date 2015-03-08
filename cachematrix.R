## functions are geting inverse of an matrix. Inverse is looked up in cache .
## if inverse is not in a cache, it is calculated and cached.

## This function for matrix x makes a list:set-sets a matrix, get-gets a matrix, setinv-sets inverse of a matrix,
##getinv- gets inverse of an matrix

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


## If inverse of a matrix has  been  calculated, it is called from a cach. If it hasn't been calculated , then we use
##function solve() to find inverse and then cached it by setinv element of list.

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
