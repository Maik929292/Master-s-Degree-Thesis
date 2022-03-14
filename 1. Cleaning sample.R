library(tidyverse) #-> sarà utile per la lettura del file attraverso la funzione read_csv. 
library(jsonlite) #-> sarà utile in quanto alcune colonne sono in formato json e occorrerà separare le variabili al loro interno
library(Amelia) # -> con questo pacchetto sarà possibile avere una visualizzazione grafica dei dati mancanti
library(pastecs)#
library(plotrix)#
library(dplyr)#
library(lubridate) #
library(ggplot2) #
library(maps) #
library(RColorBrewer)#

setwd("~/R/GStore")

df <- read_csv("Data Frame Gstore sample.csv")


df <- as.data.frame(((sapply(df, function(x){ 
  x <- gsub("not available in demo dataset", NA,x)
}))), stringsAsFactors = FALSE)

#Visualizzo se ci sono dati mancanti

#tiff('missingmap.tiff', units="in", width=12, height=6, res=300)

missmap(df, main ="GStore Data - Missing Map",y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#dev.off()

#Elimino le colonne che presentano solo valori NA

df <- df %>%
  select_if(~ !all(is.na(.)))

#Modifico i valori NA della variabile transactionRevenue in 0

df$transactionRevenue = ifelse(is.na(df$transactionRevenue) == TRUE, 0, df$transactionRevenue)

#Escludo dal dataframe le variabili con un'elevata percentuale di dati mancanti

df <- select(df, -adwordsClickInfo.gclId,
             -adwordsClickInfo.slot, 
             -adwordsClickInfo.page, 
             -adwordsClickInfo.adNetworkType,
             -adwordsClickInfo.isVideoAd,
             -adContent)

df <- select(df, -keyword)

df <- select(df, -referralPath)

df <- select(df, -networkDomain)

#l 90% della variabile campaign contiene dati mancanti.
#Il 100% delle osservazioni ? Not Socially Engaged
#Elimino la variabile campaign e socialEngagementType

df <- select(df, -campaign, -socialEngagementType)

#A causa dei dati mancanti in entrambe le variabili, non utilizzo queste variabili geografiche.
#Nel dataset sono presenti altre variabili geografiche meno dettagliate, ma senza dati mancanti (continent,subContinent,country)

df <- select(df, -city, -metro, -region)

#Modifico i valori NA in 0 

df$newVisits = ifelse(is.na(df$newVisits) == TRUE, 0, df$newVisits)

df$bounces = ifelse(is.na(df$bounces) == TRUE, 0, df$bounces)

df$pageviews = ifelse(is.na(df$pageviews) == TRUE, 1, df$pageviews)


#Poiché i valori presenti indicano TRUE, modifico il valore NA con il valore FALSE.

df$isTrueDirect = ifelse(is.na(df$isTrueDirect) == TRUE, FALSE, df$isTrueDirect)

#Poiché la variabile source presenta livelli simili, possono essere raggruppati in google, youtube,direct e other

Google <- grepl("google", df$source)
Youtube <- grepl("youtube", df$source) 
Direct <- grepl("direct", df$source) 
Other <- !grepl("google",df$source) & !grepl("youtube", df$source) & !grepl("direct", df$source)

df$source[Google] <- "Google" 
df$source[Youtube] <- "Youtube"
df$source[Direct] <- "Direct"
df$source[Other] <- "Altri"

#Sono presenti osservazioni notset, che possono essere modificate in Unknown.

Unknowncont <- grepl("not set", df$continent)
Unknownsubcont <- grepl("not set", df$subContinent)
Unknowncountry <- grepl("not set", df$subContinent)

df$continent[Unknowncont] <- "Sconosciuto"
df$subContinent[Unknownsubcont] <- "Sconosciuto"
df$country[Unknowncountry] <- "Sconosciuto"

#Il 98% delle osseervazioni è composto da 6 principali sistemi operativi
#Creo la categoria "Other" per i SO non compresi nei 6 principali.

OtherOS <- !grepl("Android",df$operatingSystem) & 
  
  !grepl("Chrome OS", df$operatingSystem) &
  
  !grepl("iOS", df$operatingSystem) &
  
  !grepl("Linux", df$operatingSystem) &
  
  !grepl("Macintosh", df$operatingSystem) &
  
  !grepl("Windows", df$operatingSystem)

df$operatingSystem[OtherOS] <- "Altri OS"

levels(df$operatingSystem)[levels(df$operatingSystem) == "Windows Phone"] <- "Altri OS"


#Il 98% dei browser utilizzati Ã¨ composto da 8 Browser principali
#Creo la categoria "Other Browser" per quelli non compresi nei primi 8.

OtherBrowser <-  !grepl("Chrome",df$browser) & 
  
  !grepl("Edge", df$browser) &
  
  !grepl("Firefox", df$browser) &
  
  !grepl("Internet Explorer", df$browser) &
  
  !grepl("Opera", df$browser) &
  
  !grepl("Safari", df$browser) &
  
  !grepl("Webview", df$browser) 

df$browser[OtherBrowser] <- "Altri"

#Modifico le osservazioni "not set" in "none" per creare una sola categoria.

df$medium <- sub("(not set)", "none", df$medium)

#Modifico il nome delle osservazioni none in Unknown

Unknownmedium <- grepl("none", df$medium) 

df$medium[Unknownmedium] <- "Sconosciuto"

#Modifico la classe delle variabili

df$date <- as.Date(df$date, format("%Y%m%d"))

df$visitStartTime <- as.POSIXlt(as.numeric(df$visitStartTime), origin="1970-01-01", tz="UTC")

df$visitNumber <- as.numeric(df$visitNumber)

df$isMobile <- as.logical(df$isMobile)

df$visits <- as.numeric(df$visits)

df$hits <- as.numeric(df$hits)

df$channelGrouping <- factor(df$channelGrouping)

df$browser <- factor(df$browser)

df$operatingSystem <- factor(df$operatingSystem)

df$medium <- factor(df$medium)

df$deviceCategory <- factor(df$deviceCategory)

df$continent <- factor(df$continent)

df$subContinent <- factor(df$subContinent)

df$country <- factor(df$country)

df$source <- factor(df$source)

df$pageviews <- as.numeric(df$pageviews)

df$isMobile <- as.logical(df$isMobile)

df$isTrueDirect <- as.logical(df$isTrueDirect)

df$newVisits <- as.logical(as.numeric(df$newVisits))

df$bounces <- as.logical(as.numeric(df$bounces))

df$transactionRevenue <- as.numeric(df$transactionRevenue)

levels(df$channelGrouping)[levels(df$channelGrouping)=="(Other)"] <- "Altri"

levels(df$browser)[levels(df$browser)=="Android Webview"]<-"Altri"
levels(df$browser)[levels(df$browser)=="Safari (in-app)"]<-"Altri"
levels(df$browser)[levels(df$browser)=="Opera"]<-"Altri"
levels(df$browser)[levels(df$browser)=="Opera Mini"]<-"Altri"
levels(df$browser)[levels(df$browser)=="Other"]<-"Altri"
levels(df$browser)[levels(df$browser)=="IE with Chrome Frame"] <- "Altri"

levels(df$subContinent)[levels(df$subContinent)=="Caribbean"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Central Asia"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Eastern Africa"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Melanesia"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Micronesian Region"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Middle Africa"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Northern Africa"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Polynesia"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Southern Africa"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Western Africa"]<-'Altri'
levels(df$subContinent)[levels(df$subContinent)=="Unknown"]<-'Altri'


df$date2<- as.POSIXct(df$visitStartTime, format="%m/%d/%Y %H:%M:%S")
df$dow<- wday(df$date2)
df$tod  <- as.numeric(df$date2 - as.POSIXct(strftime(df$date2,format="%Y-%m-%d")))/3600
df$bins <- cut(df$tod,breaks=0:24,labels=F)
df$bins = ifelse(is.na(df$bins) == TRUE, 23, df$bins)   #####viene creato un NA e l'orario è le 22.41
df$month <- month(df$date)
df$newVisits[df$newVisits=="TRUE"]<-1
df$newVisits[df$newVisits=="FALSE"]<-0
df$hours <- df$bins

df <- select(df, -X1,-fullVisitorId,-sessionId,-visitId, 
             -isMobile,-visits,-isTrueDirect, -deviceCategory, -bounces,-medium,-date2,-tod,-bins)


sample <- order(df$visitStartTime)[1:(nrow(df)*0.8)]


train <- df[sample,]

test <- df[-sample,]






 





