#Juan Camilo Rivera
library(stringr)
library(dplyr)

rm(list = ls())

#Fill values with mean of days
#Parameters
#@Data with two columns, Date and Value
data <- read.table("DATOS_FORMATO/Melonisa_SR_WAM2.txt", header = T)

fillmissingvalues <- function(data)
{
  #Separate data
  fechas <- str_split_fixed(data$Date, "-", 3)

  #Anho
  data$anho <- fechas[,1]

  #Month
  data$month <- fechas[,2]


  #Day
  data$day <- fechas[,3]

  #Fecha
  data$Fecha <- paste0(data$month, data$day)


  #Where are NA's
  index <- which(is.na(data$Value)==T)


  #days where NA
  days_NA <- data[index, "Fecha"]

  #Data frame
  data <- data[,-3]
  data$Fecha <- paste0(data$month, data$day)
  promedio = data %>% group_by(Fecha) %>% summarise(day = mean (Value, na.rm = T))

  #Encontrar los dias para hacer el match
  index_match <- which(promedio$Fecha %in% days_NA)
  valor <- promedio[index_match, "day"]


}

