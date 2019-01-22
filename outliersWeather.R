#'Outliers weather data
#'
#'This functions finds outliers of weather variables those values are out range -4SD and 4SD.
#' @param dataWeather is data weather. It is composed by two columns Date and Value.
#' @param format_date is the format of date. It could be in hours or days. For days the format
#' data is year-month-day.
#' @param variable is weather variable.
#' @param plot If plot is TRUE, function returns a graph with outliers
#' @return a list composed by graph and data frame with outliers detected and data without outliers.
#' @export
outliersWeather <- function(dataWeather, variable, stationName, format_date = "%Y-%m-%d", plot = TRUE)
{

  #libraries
  library(ggplot2)

  #read teh file
  dataWeather[,1] <- as.Date(as.character(dataWeather[,1]), format = format_date)
  dataWeather <- unique(dataWeather)

  #Verify amount days
  first_day <- dataWeather[,1][order(dataWeather[,1])[1]]
  last_day <-  dataWeather[,1][length(order(dataWeather[,1]))]
  days <- seq(from= first_day ,  to = last_day, by= "day")
  data_complete <- data.frame( Date = days, Value = NA)

  #for ( i in 1:dim(data_complete)[1])
  #{
  #  index <- which((data_complete[i,1] == dataWeather$Date)== TRUE)
  #  data_complete[i,2] <- dataWeather[index, 2]

  #}

  merge_tablas <- merge (data_complete, dataWeather, by = "Date", all.x = T)
  merge_tablas <- merge_tablas[, -2]
  colnames(merge_tablas) <- c("Date", "Value")


  dataWeather <- merge_tablas

  #dataWeather <- data_complete



  #index <- which (data_complete$Date %in% dataWeather$Date)

  #dataWeather <- merge(data_complete, dataWeather, by = "Date", all.x = TRUE)
  #dataWeather <- data_complete


  #Station and variable name
  stationName <- as.character(stationName)
  variable <- as.character(variable)

  #Detect outliers with desviation standard
  sd_weather <- sd(dataWeather[,2], na.rm = TRUE)
  mean <- mean(dataWeather[,2], na.rm = TRUE)
  index <- which(dataWeather[,2] > mean  + 3*sd_weather | dataWeather[,2] < mean -3*sd_weather )

  #Detect outliers
  outliers  <- dataWeather[,2][index]


  #Plot
  if(plot ==  TRUE)
  {
    #Color
    dataWeather$Colour <- "Standard"
    dataWeather$Colour[index]="Outliers"

    p <- ggplot(dataWeather, aes(x=Date, y= Value, color= Colour)) +
      ggtitle(paste0("Outliers","\n", stationName, "\n", variable)) +
      geom_point() + theme(plot.title = element_text(hjust = 0.5))



    #ggplot(read_file, aes(x=Date, y= Value, color= Colour)) + geom_point() + ggtitle(paste0("Grafica de Outliers","\n", splitname[1], "\n", "Variable ", variable)) + theme(plot.title = element_text(hjust = 0.5))
    #ggsave(paste0(here::here(), "/Outliers/", splitname[1], "_", variable, ".pdf"))

    #graph
  }

  else
    {
      p <- NA
    }

  #Put NA
  dataWeather[,2][index] <- NA


  #Delete column Colour
  dataWeather$Colour <- NULL


  #Plot outliers
  summary_outliers <- list("outliers" = outliers, "ind" = index, "without_outiers" = dataWeather )
  result <- list("summary" = summary_outliers, "graph" = p )


  return((result))
}


