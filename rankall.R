rankall <- function(outcome, num = "best") {
  possible.outcomes <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  outcome.col <- possible.outcomes[[outcome]]
  if (is.null(outcome.col))
    stop("invalid outcome")
  
  
  hospital.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Convert the desired column to numeric
  hospital.df[, outcome.col] <- suppressWarnings(sapply(hospital.df[, outcome.col], as.numeric))
  
  #Make data.frame for all states
  hospital.states.df <- subset(hospital.df, select = c(outcome.col, 2, 7))
  
  hospital.split.df <- split(hospital.states.df, hospital.states.df[3])
  
  staterank <- function(hospital.state.df) {
    
    #Make list of positions
    rank.list <- order(hospital.state.df[,1],hospital.state.df[,2], na.last = NA)
    
    #Check validity of num argument and assign numeric value
    if (num == "best")
      num <- 1
    else if (num == "worst")
      num <- length(rank.list)
    else if (!is.numeric(num))
      stop("Unrecognised num argument")
    
    hospital.state.df[rank.list[num],2]
  }
  
  ranked.states <- data.frame(sapply(hospital.split.df, staterank))
  ranked.states <- data.frame(ranked.states, row.names(ranked.states))
  names(ranked.states) <- c("hospital", "state")
  ranked.states
}