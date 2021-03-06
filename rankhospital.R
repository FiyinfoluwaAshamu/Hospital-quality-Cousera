rankhospital <- function(state, outcome, num = "best") {
  possible.outcomes <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  outcome.col <- possible.outcomes[[outcome]]
  if (is.null(outcome.col))
    stop("invalid outcome")
  
  
  hospital.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Convert the desired column to numeric
  hospital.df[, outcome.col] <- suppressWarnings(sapply(hospital.df[, outcome.col], as.numeric))
  
  #Stop if state is not a possible value of states
  if (!state %in% unique(hospital.df[,7]))
    stop("invalid state")
  
  # Make data.frame for state
  hospital.state.df <- subset(hospital.df, State == state, c(outcome.col,2))
  
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
