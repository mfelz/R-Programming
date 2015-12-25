complete <- function(directory, id = 1:332) {
	  n <- length(id)
	  dir <- toString(directory)
	  
	  df <- data.frame(id = numeric(n), nobs = numeric(n))
	  for (i in 1:n) {
		val <- id[i]
	      file <- sprintf("%s\\%03d.csv", dir, val)
		dat <- read.csv(file)
	  	df$id[i] <- val
		df$nobs[i] <- sum(complete.cases(dat)[TRUE])
		
	  }
	  print(df)
}