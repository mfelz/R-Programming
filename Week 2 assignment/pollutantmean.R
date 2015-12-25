pollutantmean = function(directory, pollutant, id = 1:332) {
	  dir = toString(directory)
	  total = c()
	  files = sprintf("%s\\%03d.csv", dir, id)
	  for (file in files){
	  	dat = read.csv(file)
		total = c(total, dat[[pollutant]])
	  }
	  mean(total, na.rm = TRUE)
	  
}