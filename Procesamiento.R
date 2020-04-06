
library(gdata)


#librerias
is.leapyear=function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}


datos_origi <- read.xls("DATA_ORIGINAL/20180709100312.xlsx")

#Cargar la tabla
load("Datos_Original.RData")


#Vectores con nombres
indexRow_nombresEsta <- which(datos_origi[,1]=="FECHA DE")
indexRow_nombresVaria <- indexRow_nombresEsta - 1
IndeXRow_comiedatos <- which(datos_origi[,1]=="1")



#datos_origi[indexRow_nombresEsta,4] fecha
#index <- which(datos_origi[,1]=="FECHA DE")
#nombres_variable <- datos_origi[index-1,8]
#Temperatura maxima nombres_variable <- datos_origi[30634,6]
#nombres_estacion <- datos_origi[index,14]



creacion_datos <- function (indexRow_nombresEsta, 
                            indexRow_nombresVaria,
                            IndeXRow_comiedatos)
{ 

#data_total <- datos_origi[10:40,1:14]
data_total <- datos_origi[IndeXRow_comiedatos:(IndeXRow_comiedatos+30),1:14]
data_total <- data_total[,-2]
data_total <- as.matrix(data_total)
class(data_total) <- "numeric"

#anho

#anho <- as.character(datos_origi[3,7])
anho <- as.character(datos_origi[indexRow_nombresEsta,7])
anho <- gsub(" ", "", anho, fixed = TRUE)
anho <- as.numeric(substr(anho, nchar(anho)-3,nchar(anho)))
anho_aux <- as.Date(paste0(as.character(anho),"-01-01"), "%Y-%m-%d")

data_day <- seq(anho_aux,length.out = 365, by= "day")

#verificar si el anho es bisiestro

bisestro <- is.leapyear(anho)

#matriz to vector 
vector_day <- as.vector(data_total[,2:13])


#eliminar los dias de los meses que son de 30
if(bisestro== TRUE)
{
  eliminar_days <- c(61,62,124,186,279,341)
}
 
eliminar_days <- c(60,61,62,124,186,279,341)

vector_day <- vector_day[-eliminar_days]
final_data <- data.frame(Date=data_day, Value= vector_day )


#nombre de la estacion
nombre_esta <- datos_origi[indexRow_nombresEsta,14]
nombre_esta <- gsub(" ", "", nombre_esta, fixed = TRUE)

#nombre de la variable
#indexRow_nombresVaria
nombre_var <- datos_origi[indexRow_nombresVaria,8]
nombre_var <- gsub(" ", "", nombre_var, fixed = TRUE)

if(nombre_var == "TEMPERATU")
{
  tempera <- datos_origi[indexRow_nombresVaria,6]
  tempera <- gsub(" ", "", tempera, fixed = TRUE)
  
  if(tempera == "AXIMOSDI")
  {
    nombre_var <- "TX"
  }
  
  if(tempera == "INIMOSDI")
  {
    nombre_var <- "TM"
  }
  
}

#unidades
unidades <- "XX"



#nombre de las estaciones

if(nombre_esta == "GANA")
{
  nombre_esta <- "TRIGANA"
}

if(nombre_esta == "ENALA")
{
  nombre_esta <- "LorenaLa"
}


if(nombre_esta == "BAN")
{
  nombre_esta <- "Uniban"
}


if(nombre_esta == "CANALA")
{
  nombre_esta <- "ToscanaLa"
}


if(nombre_esta == "DOMAR")
{
  nombre_esta <- "Pradomar"
}


if(nombre_esta == "OL")
{
  nombre_esta <- "Eupol"
}

if(nombre_esta == "OLOSCED")
{
  nombre_esta <- "AptoLosCedros"
}


if(nombre_esta == "COEL")
{
  nombre_esta <- "CascoEl"
}


if(nombre_var == "E VELOCID")
{
  nombre_esta <- "VM"
  unidades <- "MS"
}

if(nombre_var == "VELOCIDAD")
{
  nombre_esta <- "VV"
  unidades <- "MS"
}


if(nombre_var == "RECORRIDO")
{
  nombre_var <-"RV"
  unidades <- "KM"
}


if(nombre_var == "PRECIPITA")
{
  nombre_var <-"P"
  unidades <- "MM"
}

if(nombre_var == "HUMEDADR")
{
  nombre_var <-"RH"
  unidades <- "NE"
}


if(nombre_var == "TX" | nombre_var == "TM")
{
  unidades <- "CD"
}

result_table <- as.data.frame(final_data)

result_table$name_estacion <-  nombre_esta
result_table$name_variable <-  nombre_var
result_table$unidades <-  unidades


return(result_table) 
 
#nombre variable
#write.table (final_data, paste0("./DATA/",nombre_esta,"_",nombre_var, "_", unidades,".txt"), row.names = FALSE)

}

indexRow_nombresEsta=indexRow_nombresEsta
indexRow_nombresVaria=indexRow_nombresVaria
IndeXRow_comiedatos = IndeXRow_comiedatos

lista_files <- list()

for (i in 1: length(IndeXRow_comiedatos))
{

  lista_files[[i]] <- creacion_datos(indexRow_nombresEsta=indexRow_nombresEsta[i],
                                     indexRow_nombresVaria=indexRow_nombresVaria[i],
                                     IndeXRow_comiedatos = IndeXRow_comiedatos[i])
}


#Lista completa
list_complete <- do.call("rbind", lista_files)

#Save
#save(list_complete, file = "Lista_Completa.RData")

#Cargar tabla
load("Lista_Completa.RData")

#
names_stations <- unique(list_complete$name_estacion)
names_variables <- unique(list_complete$name_variable)
list_complete$name_variable <- as.factor(list_complete$name_variable)

#save

save_file <- function (list_complete, variable, station, unidades)
{
  dataPer_station <- subset(list_complete, name_estacion == station & name_variable == variable)
  dataPer_station$name_estacion <- NULL
  dataPer_station$name_variable <- NULL
  dataPer_station$unidades <- NULL
  
  write.table (dataPer_station, paste0("./DATA/",station,"_",variable, "_", unidades,".txt"), row.names = FALSE)
  
}


for(i in 1:9)
{
  save_file(list_complete, "RH", names_stations[i], "NE")
  save_file(list_complete, "P", names_stations[i], "MM")
  save_file(list_complete, "TX", names_stations[i], "CD")
  save_file(list_complete, "TM", names_stations[i], "CD")
  
  
}


