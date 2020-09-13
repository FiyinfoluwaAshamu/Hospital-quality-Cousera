best <- function(state, outcome){
  
  data <- read.csv("outcome-of-care-measures.csv", header =TRUE, stringsAsFactors=F)
  statevector <- unique(data$State)
  outcomevector <- c("heart attack", "heart failure", "pneumonia")
  
  #check validity
  if(!(state %in% statevector)){
    stop("Invalid state")
  }
    else if (!(outcome %in% outcomevector)){
    stop("Invalid outcome")
    
  }
  
  
  X <- data[data$State== state,]
  names(X)[c(11, 17, 23)] <- outcomevector
  answer <- X[X[,outcome] == min(X[,outcome]), ][2]    
  FA <- answer[with(answer, order(Hospital.Name)), ]
  FA[1] 
}