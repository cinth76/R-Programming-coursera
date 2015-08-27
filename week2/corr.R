source("complete.R")
corr <- function(directory, threshold = 0) {
	  correlations <- c()
	  for (i in 1:332) {
		  nr <- str_pad(i, 3, pad="0")
		  monitor <- read.csv(paste(directory, "/", nr, ".csv", sep=""))
		  all <- complete.cases(monitor)
		  if (sum(all) > threshold) {
		      correlations <- c(correlations, cor(monitor[all,"sulfate"], monitor[all,"nitrate"]))
		  }
	  }
	  correlations
}