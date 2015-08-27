pollutantmean <- function(directory, pollutant, id = 1:332) {
    count <- 0
    sum <- 0
    for (i in id) {
      nr <- str_pad(i, 3, pad="0")
      monitor <- read.csv(paste(directory, "/", nr, ".csv", sep=""))
      data <- monitor[pollutant]
      
      count <- count + colSums(!is.na(data))
      sum <- sum + colSums(data, na.rm=T)
    }
    sum/count
}