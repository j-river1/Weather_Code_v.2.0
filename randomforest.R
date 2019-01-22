library(rminer)


#Format
data_aux <- data
data <- as.data.frame(data)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data$Value <- as.numeric(data$Value)

#Subset star and end data
data <- subset(data, Date >= star_date & Date <= end_date)
data_withNA <- (as.numeric(as.character(data[,2])))
index_NA <- which(is.na(data_withNA) == TRUE)

#Where are NA's. If we assume 10 predictors.

#Case # 1. If the position the first NA is less than 10. So is neccesary complete
#values reach 10 values.

if (index_NA[1] < 10)
{

  data_withNA[index_NA[1],]





}











