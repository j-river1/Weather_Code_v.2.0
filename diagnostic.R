#'Diagnostic of weather station
#'
#'This functions returns all information about weather station, name,
#'start and end date of data,
#' total data, amount missing values, variable and units.
#' @param filepath is the file path where is file with data.
#' @param format_date is the format of date. It could be in hours or days. By default
#' data is year-month-day.
#' @return All information of station and data of weather station.
#' @author Juan Camilo Rivera Palacio. Assistant Research CIAT
diagnostic <- function(filepath, format_date = "%Y-%m-%d")
{
  file <- read.table(filepath, header = TRUE)

  #Check name file
  filename <- basename(filepath)
  split_name <- unlist(strsplit(filename, split='_', fixed=TRUE))
  split_name <- gsub("txt","",gsub("[[:punct:]]","",split_name) )

  if((split_name[2] %in% c("P", "TX", "TM", "SR", "RH"))!= TRUE)
  {
    messa_varia <- paste(split_name[2], " is not variable validated. Check name of file")
  }

  else
      {

        if(split_name[2] == "P")
        {
          namevariable <- "Precipitation"
        }
        if(split_name[2] == "TX")
        {
          namevariable <- "Maximum Temperature"
        }
        if(split_name[2] == "TM")
        {
          namevariable <- "Minimum Temperature"
        }
        if(split_name[2] == "SR")
        {
          namevariable <- "Solar Radiation"
        }
        if(split_name[2] == "RH")
        {
          namevariable <- "Relative Humidity"
        }



        messa_varia <- namevariable
      }

  if((split_name[3] %in% c("MJM2", "FD", "KWHM2", "WHM2", "MM", "NE", "WAM2", "CALCM2",
                            "CD"))!= TRUE)
  {
    mes_unit <- paste(split_name[3], " is not unit validated. Check name of file")
  }

  else
    {
      if(split_name[3] == "MJM2")
      {
        namevariable <- "Megajoules per meter square"
      }
      if(split_name[3] == "WAM2")
      {
        namevariable <- "Watts per meter square"
      }
      if(split_name[3] == "NE")
      {
        namevariable <- "None"
      }
      if(split_name[3] == "MM")
      {
        namevariable <- "Milliliters"
      }
      if(split_name[3] == "FD")
      {
        namevariable <- "Falherein Degree"
      }
      if(split_name[3] == "CD")
      {
        namevariable <- "Celsius Degree"
      }


      mes_unit <- namevariable
    }


  if(ncol(file)==2 && colnames(file)[2]== "Value")
  {
    mes_type  <- "Daily"
  }
  else if (ncol(file)==3 && colnames(file)[2]== "Hour" && colnames(file)[3]== "Value")
  {
    mes_type <- "Hourly "
  }
  else
  {
    mes_type <- "There is problem with number of columns or column name "
  }


  #Check start and end
  if(any((colnames(file) %in% c("Date", "Value"))!= TRUE))
  {
    mes_date <- "There is a problem with column names"
  }

  else
  {
    file[,1] <- as.Date(as.character(file[,1]), format = format_date)
    #star_data <- sort(file[,1])[1]
    #star
    anho <- format(sort(file[,1])[1], "%Y")
    star_data  <- as.Date(paste0(anho, "-1-1"), format ="%Y-%m-%d")

    #end
    anho_final <- format(sort(file[,1], decreasing = T)[1], "%Y")
    #end_data  <- sort(file[,1], decreasing = T)[1]
    end_data <- as.Date(paste0(anho_final, "-1-1"), format ="%Y-%m-%d")
    mes_date <- paste(star_data, " to ", end_data)
    total_day <- as.numeric(end_data - star_data)

    #Total NA
    summary_date <- data.frame(Date = seq(star_data, end_data,  by = "days"))

    #Merge table
    total_table <- merge(summary_date, file, by = "Date", all.x = T)

    na <- length(which(is.na(total_table$Value)=="TRUE"))

    #Rate of missing values
    rate <- round(na/total_day*100, 2)

  }


  diagnostic_final <- data.frame(Name_station = split_name[1],
                                 variable_names = messa_varia,
                                 units = mes_unit,
                                 type = mes_type,
                                 period = mes_date,
                                 Nas = na,
                                 total_days = total_day,
                                 rate_missing = paste0(rate, "%"))


  #extract number of days and missing values

  amount_na <- vector()
  auxyear <- total_table
  auxyear$Year <- strptime(as.character(auxyear$Date), "%Y-%m-%d")
  auxyear$Year <- format(auxyear$Date, "%Y")
  auxyear$month <- format(auxyear$Date, "%m")
  years <- unique(auxyear$Year)
  months <- unique(auxyear$month)

  total_NA <- matrix(ncol = 13, nrow = length(years))
  colnames(total_NA) <- c("Jan", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Agu", "Sep", "Oct", "Nov", "Dic", "Porcentage_NA")
  rownames(total_NA) <- years
  suma <- vector()
  for (i in 1:length(years))
  {
    #Split per years
    #auxyear$Date <- format(auxyear$Date, "%Y")

    splityears <- subset(auxyear, years[i] == Year)

    for( j in 1:length(months))
    {
      #splityears$month <- strptime(as.character(auxyear$Date), "%Y-%m-%d")
      #splityears$month <- as.Date(splityears$Date, format ="%m")

      split_month <- subset(splityears, months[j] == month)
      #amount_na[j] <- length (which(is.na(split_month$Value)==T))
      total_NA [i,j] <- length (which(is.na(split_month$Value)==T))
    }
    total_NA [i, 13] <- nrow(splityears)

  }

for (i in 1:nrow(total_NA))
{
  total_NA [i, 13] <- sum(total_NA[i,])/(total_NA[i,13]) - 1
  #total_NA [i, 13] <- (total_NA[i,13])
}


  result <- list("diagonostic"= diagnostic_final, "data" =total_table, "resume" = total_NA)




  cat("\nDiagnostic of station\n\n",
      "Name of station:\t", split_name[1], "\n",
      "Variable       :\t",  messa_varia, "\n",
      "Units          :\t", mes_unit, "\n",
      "Type           :\t", mes_type, "\n",
      "Period         :\t", mes_date, "\n",
      "NA             :\t", na,"\n",
      "Total days     :\t", total_day, "\n",
      "Rate Missing   :\t", paste0(rate, "%"), "\n",
      "Resume of NA's :\t","\n" )

  prmatrix(total_NA)
  #return(invisible(result))
  return(total_table )

}
