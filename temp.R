intervalNA <- unique(data$interval[is.na(data$steps)])
for (x in intervalNA) {
    calcMean <- mean(data$steps[data$interval == x], na.rm=TRUE)
    data$steps[data$interval == x & is.na(data$steps)] <- calcMean
}