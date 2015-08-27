best <- function(state, outcome) {
	  ## Read outcome data
	  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	  
	  ## Check that state and outcome are valid
	  cols <- c(11, 17, 23)
	  names(cols) <- c('heart attack', 'heart failure', 'pneumonia')
	  colNum <- cols[outcome]
	  if (is.na(colNum)) {
	    stop('invalid outcome')
	  }
	  x[, colNum] <- suppressWarnings(as.numeric(x[, colNum]))

	  stateIdx <- x$State == state
	  if (sum(stateIdx) == 0) {
	    stop('invalid state')
	  }

	  ## Return hospital name in that state with lowest 30-day death
	  ## rate
	  minOutcome <- min(x[stateIdx,][,colNum], na.rm=T)	  
	  hospitals <- x[stateIdx & !is.na(x[colNum]) & x[colNum] == minOutcome,][,2]

	  sort(hospitals)[1]
	  
}