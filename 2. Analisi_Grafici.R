library(tidyverse)# #-> sarà utile per la lettura del file attraverso la funzione read_csv. 
library(jsonlite)# #-> sarà utile in quanto alcune colonne sono in formato json e occorrerà separare le variabili al loro interno
library(Amelia)#
library(pastecs)#
library(plotrix)#
library(dplyr)#
library(lubridate) #
library(ggplot2) #
library(maps) #
library(RColorBrewer)#

str(train)

train$ln_Rev=log(train$transactionRevenue+1)
test$ln_Rev=log(test$transactionRevenue+1)

train$transactionRevenue <- train$transactionRevenue/1000000
test$transactionRevenue <- test$transactionRevenue/1000000

train <- train[order(train$ln_Rev),]

#tiff('LogRicaviDistrib.tiff', units="in", width=10, height=5, res=300)

plot(train$ln_Rev,main="Distribuzione dei ricavi (in scala logaritmica)", ylab= "Log Ricavi", xlab= "n")

#dev.off()

### solo per i valori maggiori di zero

train4<-subset(train3, ln_tr_rev>0)
train5 <- subset(train, ln_Rev>0)
train6 <- subset(train, transactionRevenue>0)


#Frequenze Browser

train$browser <- with(train, factor(browser, levels = c('Altri', 'Chrome','Internet Explorer', 'Safari', 'Edge', 'Firefox')))

t_browsers<-table(train$browser)
tab_brows<-round(100*prop.table(t_browsers),2)

#tiff('browserpie.tiff', units="in", width=9, height=7.8, res=300)

coul <- brewer.pal(6, "Set1") 

browspie <- pie(tab_brows, labels =c('Altri (3,82%)', 'Chrome (67,66%)','Edge (1%)', 'Safari (21,06%)',
                                      'Internet Explorer (2,06%)', 'Firefox (4,40%)'), col = coul)

#dev.off()

#####Revenue Browser solo browser con Ricavi>0

trainBrowserRev<- train5%>%
  group_by(Browser = browser) %>%
  summarize(Ricavi = sum(transactionRevenue), LogRicavi = sum(ln_Rev))

View(trainBrowserRev)

train6%>%
  group_by(Browser = browser)%>%
  as.data.frame()%>%
  ggplot(aes(Browser, ln_Rev))+
  geom_boxplot(outlier.colour="red", outlier.shape=4,
               outlier.size=3.5,aes(fill = Browser))+
  stat_summary(fun=mean, geom="point", shape=8, size=4)+
  scale_color_brewer(palette="Set3")+ guides(fill = F)+
  labs(x = "Browser", y = "Log Ricavi")+
  theme_light()
  
#dev.off()


################################## Channel Grouping ##################################
### Channel through which user get to stores

#tiff('ChannVis.tiff', units="in", width=10, height=5, res=300)

ggplot(train, aes(channelGrouping, fill=channelGrouping))+
  geom_bar() + scale_fill_brewer(palette = "RdYlBu")+guides(fill=F)+
  ylab("n")  + labs(title = "Distribuzione delle visite per Canale", y = "N° Visite", x = "Canale")


#dev.off()

#tiff('ChannRevenue.tiff', units="in", width=10, height=5, res=300)

train %>%
  group_by(Channel = channelGrouping) %>%
  summarise(Count = n(),transactionRev = sum(transactionRevenue))%>%
  ggplot(aes(Channel,transactionRev, fill = Channel))+
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "RdYlBu")+guides(fill=F)+
  labs(x = "Canale", y = "Ricavi ($)",
       title = "Distribuzione dei ricavi per Canale")

#dev.off()

######################### Sistemi Operativi ########################################

#Pie OS
t_os<-table(train$operatingSystem)
tab_os<-round(100*prop.table(t_os),2)
View(tab_os)
lab_os<- rownames(tab_os)
x_os=tab_os[1:8]

#### individuo e Raggruppo OS con quota minore di 1%
### costruisco altra tabella

sum(x_os[which(x_os<1)])
tab_os2<-matrix(0,7,1)
tab_os2[2:7]<-tab_os[c(1,2,3,4,5,7)]
tab_os2[1]<-sum(x_os[which(x_os<1)])

train$operatingSystem <- with(train, factor(operatingSystem, 
                                            levels=c('Altri OS','Android','Chrome OS',
                                                    'iOS', 'Linux', 'Macintosh', 'Windows')))

#tiff('OSpie.tiff', units="in", width=8, height=7.8, res=300)
coul2 <- brewer.pal(7, "Set3")
OSpie <- pie(tab_os2, labels= c('Altri OS (0,67%)', 'Chrome OS (2,62%)', 'Android (12,62 %)',
                                'iOS (11,12%)', 'Linux (3,65%)', 'Macintosh (29,05%)', 'Windows (40,27%)'), col = coul2)
#dev.off()

#tiff('OSRev.tiff', units="in", width=10, height=5, res=300)

train %>%
  group_by(OS = operatingSystem) %>%
  summarise(Count = n(),transactionRev = sum(transactionRevenue))%>%
  ggplot(aes(OS,transactionRev, fill = OS))+
  geom_bar(stat="identity")+guides(fill=F)+
  labs(x = "Sistema Operativo", y = "Ricavi ($)",
       title = "Distribuzione dei ricavi per Sistema Operativo")+
  scale_fill_brewer(palette = "Set3")

#dev.off()

######### Top sub continents
###################################### Subcontinent ##########################################
levels(train$subContinent)[levels(train$subContinent)=="Sconosciuto"]<-'Altri'

colourCount = length(unique(train$subContinent))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#tiff('SubVis.tiff', units="in", width=10, height=4, res=300)
prop.table(table(train$subContinent))%>%
  as.data.frame() %>%
  ggplot(aes(Var1, Freq, fill= Var1)) +
  geom_bar(stat="identity") +
  labs(x = "SubContinent", y = "Frequenza %", title = "Distribuzione Visite per SubContinent")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = getPalette(colourCount))+guides(fill=F)+
  coord_flip()
#dev.off()


########################## REVENUE and SALES #################

#grafico con linea temporale inserito nel file word

#tiff('Temp.tiff', units="in", width=10, height=5, res=300)
train%>%
  group_by(Date = date) %>%
  summarise(Count = n(), ln_tr_rev=sum(ln_Rev))%>%
  #summarize(Count = n()) %>%
  # select(Date, Count)%>%
  # as.data.frame()%>%
  ggplot()+
  geom_line(aes(Date,Count,colour="Visite"), size = 1)+
  geom_line(aes(Date,ln_tr_rev,colour="Log Ricavi"), size = 1)+
  scale_x_date(date_breaks = "1 month",date_labels = "%m-%Y")+
  labs(col = "Legenda" , x = "Data", y = "Visite / Ricavi (scala logaritmica)", title = "Distribuzione mensile delle visite" )
#dev.off()

Numerocol = 12

getPalette3 = colorRampPalette(brewer.pal(9, "YlOrRd"))

#tiff('MonthRev.tiff', units="in", width=10, height=5, res=300)

train %>%
  group_by(Data = (format(as.Date(train$date), "%Y-%m"))) %>%
  summarize(Count = n(), transactionRevenue = sum(transactionRevenue)) %>%
  as.data.frame()%>%
  ggplot(aes(Data, transactionRevenue, fill = as.factor(transactionRevenue))) + geom_bar(stat= "identity")+
  labs(x = "Mese", y = "Ricavi ($)", fill="Data", title = "Distribuzione mensile dei ricavi")+
  scale_fill_manual(values = getPalette3(Numerocol))+guides(fill=F)+coord_flip()

#dev.off()

  
#### creo df per heatmap

train$date2<- as.POSIXct(train$visitStartTime, format="%m/%d/%Y %H:%M:%S")
train$dow<-wday(train$date2)
train$cdow <- wday(train$date2,label=T)
train$tod  <- as.numeric(train$date2 - as.POSIXct(strftime(train$date2,format="%Y-%m-%d")))/3600
train$bins <- cut(train$tod,breaks=0:24,labels=F)
counts2 <- aggregate(transactionRevenue~bins+dow,train,sum)
counts    <- aggregate(ln_Rev~bins+dow,train,sum)
colnames(counts)[ncol(counts)] <- "Events"
colnames(counts2)[ncol(counts2)] <- "Events2"
View(counts)
View(counts2)

#tiff('DOW4.tiff', units="in", width=10, height=3, res=300)

ggplot(counts, aes(x=bins,y=8-dow))+
  geom_tile(aes(fill=Events))+
  scale_fill_gradientn(colours=brewer.pal(8,"YlOrRd"))+
     #                  breaks=seq(0,max(counts$Events),by=2))+
  scale_y_continuous(breaks=7:1,labels=c("Dom","Lun","Mar","Mer","Gio","Ven","Sab"))+
  labs(x="Ora del giorno", y="Giorno della settimana", fill = "Log Ricavi")+
  coord_fixed()
#dev.off()


CountEv <- count(train,dow,bins)

tiff('DOW4Visit.tiff', units="in", width=10, height=3, res=300)

ggplot(CountEv, aes(x=bins,y=8-dow))+
  geom_tile(aes(fill= n))+
  scale_fill_gradientn(colours=brewer.pal(8,"YlOrRd"))+
  #                  breaks=seq(0,max(counts$Events),by=2))+
  scale_y_continuous(breaks=7:1,labels=c("Dom","Lun","Mar","Mer","Gio","Ven","Sab"))+
  labs(x="Ora del giorno", y="Giorno della settimana", fill = "N* visite")+
  coord_fixed()

dev.off()





####################### World MAps

#w_heat<-train%>%
  #group_by(country= country) %>%
  #summarise(Count = n(), ln_tr_rev=sum(ln_Rev))

w_heat <- train%>%
  group_by(country = country) %>%
  summarise(Count = n(), ln_tr_rev=sum(transactionRevenue))


df.map <- map_data('world')
df.map %>%
  rename(country = region) ->
  df.map
df.map %>% glimpse()
str(df.map)

anti_join(w_heat
          ,df.map
          ,by = 'country'
)

df.map %>% 
  group_by(country) %>% 
  summarise() %>% 
  print(n = Inf)


w_heat %>% 
  mutate(country = recode(country
                          ,`Bosnia & Herzegovina` = 'Bosnia and Herzegovina'
                          #,`C???te d???Ivoire` = 'Ivory Coast '
                          ,`Congo - Kinshasa` = 'Democratic Republic of the Congo'
                          ,`Czechia` = 'Czech Republic'
                          ,`Macedonia (FYROM)` = 'Macedonia'
                          ,` Myanmar` = 'Myanmar'
                          ,`Gambia, The` = 'Gambia'
                          ,`Korea, North` = 'North Korea'
                          ,`Korea, South` = 'South Korea'
                          ,`St. Kitts & Nevis` = 'Saint Kitts'
                          ,`Saint Vincent and the Grenadines` = 'Saint Vincent'
                          ,`Trinidad & Tobago` = 'Trinidad'
                         
                          ,`United Kingdom` = 'UK'
                          ,`United States` = 'USA'
  )
  ) ->
  w_heat

anti_join(w_heat
          ,df.map
          ,by = 'country'
)

w_map_heat<-left_join(df.map,w_heat,
                     by = 'country'
)

w_map_heat %>% glimpse()


#============================

#tiff('MapCont.tiff', units="in", width=10, height=5, res=300)
w_map_heat %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = ln_tr_rev))+
  labs(title = str_c('Mappa dei Ricavi per Stato')
  ,fill = str_c('Ricavi ($)')
  )  +
scale_fill_gradientn(colors = c('#009933', '#ffff00', 'yellow', '#FFE118', '#ff8100', '#e60000')
                          ,values = scales::rescale(c(0, 5, 10, 20, 50,1000,4000)))

#dev.off()


#PageViews

#tiff('PageVisits.tiff', units="in", width=10, height=3, res=300)
prop.table(table(train$pageviews)) %>%
  as.data.frame() %>%
  head(40) %>%
  ggplot(aes(Var1, Freq*100)) + geom_point(aes(size = Freq, col = Freq))+
  labs(x = "Pagine visitate", y = "Frequenza %", title = "Frequenze per pagine visitate")+
  scale_color_gradient(low = "red", high = "green") + guides(col = F, size = F)
#dev.off()


#tiff('PageRev.tiff', units="in", width=10, height=3, res=300)

train %>%
  group_by(Pageviews = factor(pageviews)) %>%
  summarize(Count = n(), transactionRevenue = sum(transactionRevenue)) %>%
  head(40)%>%
  ggplot(aes(Pageviews,transactionRevenue)) +geom_point(aes(col = transactionRevenue), size = 4)+
  labs(x = "Pagine visitate", y = "Ricavi ($)", title = "Ricavi per pagine visitate", col = 'Ricavi ($)')+
  scale_color_gradient(low = "red", high = "green")

#dev.off()


