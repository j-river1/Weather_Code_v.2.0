#' Input missing values
#'
#' This function fills missing values of weather variables, such that maximum and minimum temperature
#' precipitation, solar irradiance and humidity relative.
#' @param  data is the weather data with units. See Manual
#' @param  method is the method for filling missing values.
#' Random forest = "rf"
#' @param  variable is the variable data
#' @param latitude is a latitude of meteorological station
#' @param longitude is a longitude meteorological station.
#' @param start_date is start date for input missing values and its type is character with format "Year-month-day". If t
#' method is CHIRPS the start date must be older than 1 january of 1981.
#' @param  end_date is end date for input missing values and its type is character with format "Year-month-day".
#' @param  savefiles_CHIRPS if is FALSE, the CHIRPS files dont save, if TRUE the file is saved with default name file.RData
#' @param  namefiles_CHIRPS the name of file savefiles_CHIRPS, it must be a character
#' @param  loadfiles_CHIRPS load the file RData CHIRPS
#' @param  filename_RData the file of RData
#' @return  A list with data without missing values and plot.
#' @author Juan Camilo Rivera Palacio
InputMissingValues <- function(data, variable, method, latitude, longitude, star_date, end_date, savefiles_CHIRPS = FALSE, namefiles_CHIRPS,loadfiles_CHIRPS = FALSE, filename_RData)
{
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


  if(method == "rf")
  {


  #Training sample
  data_withoutNA <- na.omit(as.numeric(as.character(data[,2])))


      for (i in 1:length(index_NA))
      {
          data_withoutNA <- na.omit(as.numeric(as.character(data_withNA)))
          #training sample
          d= CasesSeries(as.numeric(data_withoutNA), 1:10)


          #random forest
          forest  <- randomForest::randomForest(y~., data = d)

          d_Nas= CasesSeries(as.numeric(data_withNA), 1:10)
          location_NA <- which(is.na(d_Nas$y)==T)
          Nas <- d_Nas[location_NA,]
          prediccion <- as.numeric(predict(forest, Nas[1,]))

          #reemplazar en el anterior en el i + 1
          data_withNA[index_NA[i]] <- prediccion
      }

  data <- data.frame(Date = data$Date, Value = data_withNA)

  }


  #if (method == "rmawgen" & (variable == "TX" | variable == "TM" | variable == "P" ))
  #{
  #  TEMPERATURE_MAX, TEMPERATURE_MIN, PRECIPITATION, n_GPCA_iter = 10, n_GPCA_iteration_residuals =10, lag=2, p_prec = 3, p_test =2, choose_station, menu


  #}


  if(method == "CHIRPS")
  {
    #Work directory
    direct <- getwd()

    #Server
    server <- setwd("//dapadfs/data_cluster_4/observed/gridded_products/chirps/daily")

    #begin and year
    star_date <- as.Date(star_date, format = "%Y-%m-%d")
    end_date <- as.Date(end_date, format = "%Y-%m-%d")


    #Locations
    x_lon = longitude
    y_lat = latitude
    y_dat= data.frame(x=x_lon, y = y_lat)

    if(loadfiles_CHIRPS == TRUE)
    {
      setwd(direct)
      load(filename_RData)

      #Delete NULL in a list
      all_files[sapply(all_files, is.null)] <- NULL
      complete_data <- do.call(rbind, all_files)

      #subset
      sub_data <- subset(complete_data, Date >= star_date & Date <= end_date)



      data[index_NA,2] <- sub_data[index_NA,2]
      #data <- sub_data
      #data_withNA <- data
    }
    else
    {
      #List of files *.tiff
      files_tiff <- list.files(pattern = "*.tif")
      index_omit <- which(duplicated(as.Date(substr(files_tiff, 13, 22), format= "%Y.%m.%d"))==T)
      files_tiff <- files_tiff[-index_omit]

      all_files <- lapply(files_tiff , function(x) {

        date <- as.Date(substr(x, 13, 22), format= "%Y.%m.%d")
        index <- which((date >= star_date & date <= end_date) == T)


        if(length(index)> 0)
        {
          prec <- raster::stack(x[index])
          cor <- raster::extract(prec, y= y_dat)
          date <- substr(colnames(cor), 13, 23)
          date <- as.Date(date, format = "%Y.%m.%d")
          table <- data.frame(Date= date, Value = cor[1])
          return (table)
        }
        else

          return (NULL)
      })

      #Delete NULL in a list
      all_files[sapply(all_files, is.null)] <- NULL
      complete_data <- do.call(rbind, all_files)

      data[index_NA,] <- complete_data[index_NA,]
      data_withNA <- data

      setwd(direct)

      if(savefiles_CHIRPS == TRUE)
      {
        save(all_files, file = paste0(namefiles_CHIRPS, ".RData"))
        setwd(direct)
      }

    }

  }




  if(method == "ARIMA")
  {

    library(forecast)
    library(xts)


    #index
    indexNa <- which(is.na(data))

    #Convert to series time
    datats <- xts(data$Value, as.Date(data$Date, format = "%Y-%m-%d"))
    y <- datats

    datats <- log(datats*10)


    #fit the model
    fit <- auto.arima(datats)

    #Kalman filter
    kr <- KalmanRun(datats, fit$model)

    #NA
    indexNA <- which(is.na(data$Value))

    for (i in indexNA)
    {
      y[i] <- fit$model$Z %*% kr$states[i,]

    }

   data$Value <- as.numeric(exp(y)/10)

  }






  return(data)





}


#write.table




preci_aptoCerros <- read.table("Datos/AptoLosCedros_P_MM.txt", header = T, sep = " ")
preci_CampoExp  <- read.table("Datos/CampoExperimental_P_MM.txt", header = T, sep = " ")
preci_Casco <- read.table("Datos/CascoEl_P_MM.txt", header = T, sep = " ")
preci_Chigoro <- read.csv("Datos/Chigorodo_P_MM.txt", header = T)
preci_eupol <- read.table("Datos/Eupol_P_MM.txt", header = T, sep = " ")
preci_lorenala <- read.table("Datos/LorenaLa_P_MM.txt", header = T, sep = " ")
preci_MARTHA <- read.table("Datos/MARTHA_P_MM.txt", header = T, sep = " ")
preci_PistaIndira <- read.table("Datos/PistaIndira_P_MM.txt", header = T, sep = " ")
preci_Pradomar <- read.table("Datos/Pradomar_P_MM.txt", header = T, sep = " ")
preci_TRIGANA <- read.table("Datos/TRIGANA_P_MM.txt", header = T, sep = " ")
preci_Uniban <- read.table("Datos/Uniban_P_MM.txt", header = T, sep = " ")






