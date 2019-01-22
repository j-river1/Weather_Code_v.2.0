#'Localization_CHIRPS
#'
#'This function works for grouping stations according to grid CHIRPS of 5 Kilometers. The CHIRPS data base uses 5 km grid so stations could have
#'same geolocalization.
#' @param info_geol latitude and longitude of stations. This is data frame composed by three columns, long (longitude), lati(latitude)
#' and names (name of station).
#' @return Stations with g
#' @author Juan Camilo Rivera Palacio. Assistant Research CIAT
localization_CHIRPS <- function(infogeol)
{
  library(raster)
  #server
  server <- setwd("//dapadfs/data_cluster_4/observed/gridded_products/chirps/daily")

  #Base of grid
  Base_grid <- list.files(pattern = "*.tif")
  Base_grid <- raster(Base_grid[[1]])


  #Extract
  infogeol$Grid <- NULL

  for ( i in 1:nrow(infogeol))
  {
    infogeol[i,"Grid"] <-  cellFromXY(Base_grid, xy= c(infogeol[i,"long"],infogeol[i,"lati"]))
  }

  total <- split(infogeol,infogeol$Grid)

  return( total)
}


infogeol <- data.frame(long = c(-76.716667, -76.63333,-76.61666, -76.683333, -76.63333, -76.61661, -76.7, -76.7, -76.65, -76.6667, -76.683, -76.683),
                       lati = c( 7.8166667,        7.983333, 7.933333, 7.85,        7.883333,  7.9833333,  7.8 ,  7.73, 7.8166, 7.76667,  7.76667, 7.93333),
                       names = c("AptlosCedros", "CascoEl", "Eupol", "Lorenala", "MARTHA", "Pradomar", "Toscanala", "TRIGANA", "Uniban", "CampoExperimental",
                                "Chigorodo", "PistaIndira"))

infogeol <- read.csv("localizacion_fincas.csv", header = T)
infogeol$names <- infogeol$FINCA
infogeol$lati <- infogeol$Latitud
infogeol$long <- infogeol$Longitude
infogeol <- infogeol[, c(8,9,10)]






