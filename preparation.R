library(dplyr)
library(stringr)
require(tidyr)
library(stringr)
library(stringi)
c <- read.csv("datasets/caracteristiques_2019.csv", sep=';', header = TRUE)
l <- read.csv("datasets/lieux_2019.csv", sep=';', header = TRUE)
u <- read.csv("datasets/usagers_2019.csv", sep=';', header = TRUE)
v <- read.csv("datasets/vehicules_2019.csv", sep=';', header = TRUE)


types <- c("caracteristiques")#, "lieux", "usagers", "vehicules")
annees <- 2005:2019

data <- list("caracteristiques"=data.frame(matrix(ncol = length(names(c))+1, nrow = 0)), "lieux"=data.frame(matrix(ncol = length(names(l)), nrow = 0)), "usagers"=data.frame(matrix(ncol = 16, nrow = 0)), "vehicules"=data.frame(matrix(ncol = length(names(v)), nrow = 0)))
colnames(data$caracteristiques) <- c(names(c), 'gps') # gps a été effacé en 2019
colnames(data$lieux) <- names(l) # tout ce qui est dans 2005 est deja dans 2019
colnames(data$usagers) <- c("Num_Acc","id_vehicule","num_veh","place","catu","grav","sexe","an_nais","trajet","locp","actp","etatp","secu","secu1","secu2","secu3") # secu été divié en 3 en 2019 et id_vehicule est apparu en 2019
colnames(data$vehicules) <- names(v) # tout ce qui est dans 2005 est deja dans 2019

for(type in types){
	for(annee in annees){
		path <- paste0('datasets/', type, '_', annee, '.csv')
		
		sep <- if(annee==2019) ';' else ','
		if(type == "caracteristiques" && annee == 2009)
			sep = ';'
		
		print(paste("Process: ", type, annee))
		nextdata <- read.csv(path, sep=sep, header = TRUE)
		nextdata$an <- annee
		if(type == "caracteristiques" && annee != 2019 ){
			
			#format 0294992 5055737
			nextdata$long <- as.numeric(sub("(.{2})(.*)", "\\1.\\2", nextdata$long))
			nextdata$lat <- as.numeric(sub("(.{2})(.*)", "\\1.\\2", nextdata$lat))
		}
		if(type == "caracteristiques" && annee != 2019){
			nextdata[(nextdata$dep < 971) & (nextdata$dep != 201) & (nextdata$dep != 202),]$dep  <- nextdata[(nextdata$dep < 971) & (nextdata$dep != 201) & (nextdata$dep != 202),]$dep / 10
			nextdata$dep[nextdata$dep == 201] <- '2A'
			nextdata$dep[nextdata$dep == 202] <- '2B'
		}
		if(type == "caracteristiques" && annee == 2019){
			#sous la forme 48,9307000 pour 2019
			nextdata$long <- as.numeric(str_replace(nextdata$long, ',', '.'))
			nextdata$lat <- as.numeric(str_replace(nextdata$lat, ',', '.'))
			
			nextdata$com <- as.integer(gsub('^.{2}', '', nextdata$com))
		}
		if(type=="lieux"){
			nextdata$voie <- as.character(nextdata$voie)
		}
		if(type=="lieux"){
			nextdata$voie <- as.character(nextdata$voie)
			nextdata$pr <- gsub("\\(|\\)", "", as.character(nextdata$pr))
			nextdata$pr1 <- gsub("\\(|\\)", "", as.character(nextdata$pr1))
		}
		if(type=="usagers"){
			nextdata$actp <- as.character(nextdata$actp)
		}
		data[[type]] <- dplyr::bind_rows(data[[type]], nextdata) #permet un fusion par ligne avec NA pour les colonne manquantes
	
	}
}

# Netoyage et ameliorations

# separation de hrmn en hr, mn
data$caracteristiques$hrmn
data$caracteristiques$mn <- data$caracteristiques$hrmn %% 100 # garder les 2 derniers digits
data$caracteristiques$hr <- as.integer(gsub('.{2}$', '', data$caracteristiques$hrmn)) # supprimer les 2 dernier digit
data$caracteristiques$hr[is.na(data$caracteristiques$hr)] <- 0
#data$caracteristiques[,c('hrmn', 'hr', 'mn')] #pour verifier
data$caracteristiques$hrmn <- NULL



write.csv(data$caracteristiques, "caracteristiques.csv", sep=",", row.names=FALSE)
write.csv(data$lieux, "lieux.csv", sep=",", row.names=FALSE)
write.csv(data$usagers, "usagers.csv", sep=",", row.names=FALSE)
write.csv(data$vehicules, "vehicules.csv", sep=",", row.names=FALSE)





