rankall <- function(outcome, num = "best") {
	result <- c()
	states <- append(state.abb, "DC")
	states <- states[order(states)]
	for (state in states) {
		hospital <- rankhospital(state,outcome, num)
		result <- append(result,hospital)
	}
	results.frame <- data.frame(result, states)
	names(results.frame) = c("hospital", "state")
	results.frame
}