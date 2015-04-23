## the function compares 2 matrices to test whethere they are equal
matequal <- function(x, y){
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

makeCacheMatrix<-function(m = matrix()){
  
  
  set<-function(y){
    if(!is.null(original)){
      ## if matrix is not equal to chched one store its inverse and the source matrix
      if(!matequal(original,y)){
        inverted<<-NULL
        original<<-y
      }
    }else{
      inverted<<-NULL
      original<<-y
    }
    
  }
  ##retrieves  the source matrix
  get<-function()original
  ##stores the inverted matrix
  setInverted<-function(i){
    inverted<<-i
  }
  ##retrieves the inverted matrix
  getInverted<-function(){
    inverted
  }
  list(set=set
       ,get=get
       ,setInverted=setInverted
       ,getInverted=getInverted)
  
}

cacheSolve<-function(x, ...){
  m<-makeCacheMatrix()
  mat <- m$set(x)
  
  
  inv <- m$getInverted()
  if(!is.null(inv)){
    inv<- Solve(x)
    m$setInverted(inv)
  }
}