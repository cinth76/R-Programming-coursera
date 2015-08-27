library("stringr")
complete <- function(directory, id = 1:332) {
    stats <- data.frame()
    for (i in id) {
	nr <- str_pad(i, 3, pad="0")
	monitor <- read.csv(paste(directory, "/", nr, ".csv", sep=""))
	stats <- rbind(stats, c(i, sum(complete.cases(monitor))))
    }
    setNames(stats, c("id", "nobs"))
}