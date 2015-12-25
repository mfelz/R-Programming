corr <- function(directory, threshold = 0) {
	  corrs = as.numeric(c())
	  dir <- toString(directory)
	  for (ids in 1:332) {
		file <- sprintf("%s\\%03d.csv", dir, ids)
		dat <- read.csv(file)
		dat <- dat[complete.cases(dat),]
		if (nrow(dat) > threshold) {
			temp <- cor(dat["nitrate"], dat["sulfate"])
			corrs = c(corrs,temp)
		}  
	  }
	  corrs	  
}