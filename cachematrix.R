matequal <- function(x, y){
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

makeCacheMatrix<-function(m = matrix()){
  
  
  set<-function(y){
    if(!is.null(original)){
      if(!matequal(original,y)){
        inverted<<-NULL
        original<<-y
      }
    }else{
      inverted<<-NULL
      original<<-y
    }
    
  }
  get<-function()original
  setInverted<-function(i){
    inverted<<-i
  }
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