rankhospital <- function(state,outcome, num = "best") {
	## read outcome data
	ind <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	col <- ind[outcome] 
	data[, 11] <- as.numeric(data[, 11])
	data[, 17] <- as.numeric(data[, 17])
 	data[, 23] <- as.numeric(data[, 23])
	## Check that state and outcome are valid
	states = append(state.abb, "DC")
	if (is.na(match(state, states))) {
		stop("invalid state")
	}
	if (is.na(match(outcome, c("heart attack", "heart failure", "pneumonia")))){
		stop("invalid outcome")
	}
	##Return hospital name in that state with lowest 30-day death rate
	data <- data[data[,7] == state,]		##clean data by state
	data <- data[data[,col]!="Not Available",]##get rid of NA vals
	data <- data[!is.na(data[,2]),]
	data <- data[order(data[col],data[2]),]	##sort
	
	if (num == "best"){ 
		num <- 1
	} else if (num == "worst") {
		num <- nrow(data)
	} else {
		num <- num
	}
	data[num,2]			##return hospital
}