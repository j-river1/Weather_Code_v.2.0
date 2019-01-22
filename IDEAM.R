preci_aptoCerros <- read.table("Datos/AptoLosCedros_P_MM.txt", header = T, sep = " ")
preci_CampoExp  <- read.table("Datos/CampoExperimental_P_MM.txt", header = T, sep = " ")
preci_Casco <- read.table("Datos/CascoEl_P_MM.txt", header = T, sep = " ")
preci_Chigoro <- read.csv("Datos/Chigorodo_P_MM.txt", header = T, sep="\t")
preci_eupol <- read.table("Datos/Eupol_P_MM.txt", header = T, sep = " ")
preci_lorenala <- read.table("Datos/LorenaLa_P_MM.txt", header = T, sep = " ")
preci_MARTHA <- read.table("Datos/MARTHA_P_MM.txt", header = T, sep = " ")
preci_PistaIndira <- read.table("Datos/PistaIndira_P_MM.txt", header = T, sep = " ")
preci_Pradomar <- read.table("Datos/Pradomar_P_MM.txt", header = T, sep = " ")
preci_TRIGANA <- read.table("Datos/TRIGANA_P_MM.txt", header = T, sep = " ")
preci_Uniban <- read.table("Datos/Uniban_P_MM.txt", header = T, sep = " ")
tm_AptoCedros <- read.table("Datos/AptoLosCedros_TM_CD.txt", header = T, sep = " ")

outliers_aptoCerros <- outliersWeather(preci_aptoCerros, "P", "AptoLosCedros")
Input_aptoCerros <- InputMissingValues(outliers_aptoCerros$summary$without_outiers, "P", "CHIRPS", 7.8167, -76.7167, "1981-01-01", "2017-12-31")

outlierstm_aptoCerros <- outliersWeather(tm_AptoCedros, "TM", "AptoLosCedros")
Input_tmaptoCerros <- InputMissingValues(outlierstm_aptoCerros$summary$without_outiers, "TM", "ARIMA", 7.8167, -76.7167, "1981-01-01", "2017-12-31")



outliers_Casco <- outliersWeather(preci_Casco, "P", "CascoEl")
Input_Casco <- InputMissingValues(outliers_Casco$summary$without_outiers, "P", "rf", 7.8167, -76.7167, "1981-01-01", "2017-12-31")
write.table(Input_Casco, "CascoEl_P_MM.txt", row.names = F)

outliers_Chigoro <- outliersWeather(preci_Chigoro, "P", "Chigoro")
Input_Chigoro <- InputMissingValues(outliers_Chigoro$summary$without_outiers, "P", "CHIRPS", 7.766667, -76.68333, "2011-03-28", "2018-08-20")
write.table(Input_Chigoro, "Chigorodo_P_MM.txt", row.names = F)




outliers_Eupol <- outliersWeather(preci_eupol, "P", "Eupol")
Input_Eupol <- InputMissingValues(outliers_Eupol$summary$without_outiers, "P", "CHIRPS", 7.9333333, -76.616667, "1981-01-01", "2017-12-31")
write.table(Input_Eupol, "Datos_Completos/Eupol_P_MM.txt", row.names = F)



outliers_Lorenala <- outliersWeather(preci_lorenala, "P", "LorenaLa")
Input_Lorenala <- InputMissingValues(outliers_Lorenala$summary$without_outiers, "P", "CHIRPS", 7.85, -76.6833, "1981-01-01", "2017-12-31")
write.table(Input_Lorenala, "Datos_Completos/Lorenala_P_MM.txt", row.names = F)


outliers_PistaIndira <- outliersWeather(preci_PistaIndira, "P", "PistaIndira")
Input_Martha <- InputMissingValues(outliers_PistaIndira$summary$without_outiers, "P", "CHIRPS", 7.93333, -76.6833333, "2011-01-01", "2018-08-20")
write.table(Input_Martha, "Datos_Completos/PistaIndira_P_MM.txt", row.names = F)



outliers_Pradomar <- outliersWeather(preci_Pradomar, "P", "Pradomar")
Input_Pradomar <- InputMissingValues(outliers_Pradomar$summary$without_outiers, "P", "CHIRPS", 7.98333, -76.6333, "1981-01-01", "2017-12-31")
write.table(Input_Pradomar, "Datos_Completos/Pradomar_P_MM.txt", row.names = F)


outliers_Trigana <- outliersWeather(preci_TRIGANA, "P", "Trigana")
Input_Trigana <- InputMissingValues(outliers_Trigana$summary$without_outiers, "P", "CHIRPS", 7.73333, -76.7, "1981-01-01", "2017-12-31")
write.table(Input_Pradomar, "Datos_Completos/TRIGANA_P_MM.txt", row.names = F)


preci_Uniban <- read.table("Datos/Uniban_P_MM.txt", header = T, sep = " ")
outliers_Uniban <- outliersWeather(preci_Uniban, "P", "Uniban")
Input_Uniban <- InputMissingValues(outliers_Uniban$summary$without_outiers, "P", "CHIRPS", 7.81666, -76.65, "1981-01-01", "2017-12-31")
write.table(Input_Uniban, "Datos_Completos/Uniban_P_MM.txt", row.names = F)


preci_MARTHA <- read.table("Datos/MARTHA_P_MM.txt", header = T, sep = " ")
outliers_MARTHA <- outliersWeather(preci_MARTHA, "P", "MARTHA")
Input_MARTHA <- InputMissingValues(outliers_MARTHA$summary$without_outiers, "P", "CHIRPS", 7.8833333, -76.633333, "1981-01-01", "2017-12-31")
write.table(Input_MARTHA, "Datos_Completos/MARTHA_P_MM.txt", row.names = F)

tm_aptoCedros <- read.table("Datos/AptoLosCedros_TM_CD.txt", header = T, sep = " ")
outliers_tm_aptoCedros <- outliersWeather(tm_aptoCedros, "TM", "AptoLosCedros")
Input_tm_aptoCedros <- InputMissingValues(outliers_tm_aptoCedros$summary$without_outiers[360:390,], "TM", "rf", 7.8833333, -76.633333, "1983-01-01", "2017-12-31")
write.table(Input_tm_aptoCedros, "Datos_Completos/AptoLosCedro_TM_MM.txt", row.names = F)






