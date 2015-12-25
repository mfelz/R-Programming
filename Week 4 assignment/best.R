best <- function(state,outcome) {
	## read outcome data
	ind <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	col <- ind[outcome] 
	
	## Check that state and outcome are valid
	if (is.na(match(state, state.abb))) {
		stop("invalid state")
	}
	if (is.na(match(outcome, c("heart attack", "heart failure", "pneumonia")))){
		stop("invalid outcome")
	}
	##Return hospital name in that state with lowest 30-day death rate
	data <- data[data[,7] == state,]		##clean data by state
	data <- data[order(data[2]),]			##sort data on hosp name alpha
	data <- data[!is.na(data[,col]),]		##get rid of NA vals
	data[which.min(data[,col]),2]			##return hospital
}