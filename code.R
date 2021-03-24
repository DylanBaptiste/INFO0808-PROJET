install.packages(c('ggplot2', 'dplyr', 'lubridate', 'plotly'))

library("ggplot2")
library("dplyr")
library("lubridate")
library("plotly")

# ================================== ======= ================================== #
# ================================== PLOT OK ================================== #
# ================================== ======= ================================== #
caracteristiques <- read.csv("clean_datasets/accidents.csv", sep=',', header = TRUE)


str(caracteristiques)
d1 <- setNames(data.frame(table(as.Date(paste(caracteristiques$an, caracteristiques$mois, caracteristiques$jour, sep='-')))),c("Date","Count"))
d1$day <- weekdays(as.Date(d1$Date))

d2 <- d1 %>%
  mutate(Mois=month(Date), Date = as.Date(paste(year(Date), month(Date), '01', sep = '-'))) %>%
  group_by(Date, Mois) %>%
  summarise(Count = sum(Count))

d3 <- d1 %>%
  mutate(Date = month(Date)) %>%
  group_by(Date) %>%
  summarise(Count = sum(Count))

ggplotly(ggplot(d2, aes(x=Date, y=Count)) + geom_line() + geom_smooth(formula=y~x, method = 'loess') + ggtitle("Nombre d'accidents de 2005 � 2020")+ylab("Nombre d'accidents"))
plot_ly(d3, x=~Date) %>% add_lines(y=~Count) %>% layout(title = "Nombre d'accident en fonction des mois de la semaines de 2005 � 2019",xaxis = list(title="Mois de l'annee"), yaxis = list(title="Nombre d'accidents"))

plot_ly(d1, x=~day) %>% add_boxplot(y=~Count) %>% layout(title = "Nombre d'accident en fonction des jours de la semaines de 2005 � 2019", yaxis = list(title="Nombre d'accidents"), xaxis = list(title = "Jours de la semaine",categoryorder = "array", categoryarray = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")))



plot_ly(d1, x=~factor(month.abb[month(Date)], levels = month.abb)) %>% add_boxplot(y=~Count, color=~factor(day , levels=c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))) %>% layout(title = "Nombre d'accident en fonction des mois de l'annee de 2005 � 2019",xaxis=list(title="Mois de l'annee"), yaxis=list(title="Nombre d'accidents"),boxmode = "group")
plot_ly(d1, x=~day) %>% add_boxplot(y=~Count, color=~factor(month.abb[month(Date)], levels = month.abb)) %>% layout(title = "Nombre d'accident en fonction des jours de la semaines de 2005 � 2019",xaxis=list(title="Jours de la semaine"), yaxis=list(title="Nombre d'accidents"),boxmode = "group")

plot_ly(d1, x=~day(Date)) %>% add_boxplot(y=~Count) %>% layout(title = "Nombre d'accident en fonction du mois de l'annee de 2005 � 2019",xaxis=list(title="Jours du mois", yaxis=list(title="Nombre d'accidents")))
d4 <- d1 %>%
  mutate(Mois=month(Date), Day=day(Date), p=paste(month(Date), day(Date)), Date = as.Date(paste(year(Date), month(Date), day(Date), sep = '-'))) %>%
  group_by(Day, Mois, p, Date) %>%
  summarise(Count = sum(Count))
d4 <- d4[ with(d4, order(Mois, Day)), ]

plot_ly(d2, x=~factor(month.abb[Mois], levels = month.abb)) %>% add_boxplot(y=~Count)%>% layout(title = "Nombre d'accident en fonction des mois de l'annee de 2005 � 2019",xaxis = list(title="Mois de l'annee"), yaxis = list(title="Nombre d'accidents")) #par mois
plot_ly(d1, x=~week(Date)) %>% add_boxplot(y=~Count)%>% layout(title = "Nombre d'accident en fonction du numero de la semaine de 2005 � 2019",xaxis = list(title="Numero de la semaine"), yaxis = list(title="Nombre d'accidents")) #par semaine
plot_ly(d4, x=~p) %>% add_boxplot(y=~Count) %>% layout(xaxis = list(categoryorder = "array", categoryarray =d4$p))%>% layout(title = "Nombre d'accident en fonction des jours de l'annee de 2005 � 2019",xaxis = list(title="Jour de l'ann�e"), yaxis = list(title="Nombre d'accidents")) #par jour
ggplot(caracteristiques, aes(x=dep)) + geom_bar()+ggtitle("Nombre d'accidents par departement")+ xlab("Numero de departement")+ylab("Nombre d'accident")
ggplot(caracteristiques, aes(x=an)) + geom_bar()+ggtitle("Nombre d'accident par an")+ xlab("Annee")+ylab("Nombre d'accident")+ geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
ggplot(caracteristiques, aes(x=mois)) + geom_bar()+ggtitle("Nombre d'accident par mois")+ xlab("Mois")+ylab("Nombre d'accident")+ geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
ggplot(caracteristiques, aes(x=jour)) + geom_bar()+ggtitle("Nombre d'accident par jours")+ xlab("Numero du jour")+ylab("Nombre d'accident")
ggplot(caracteristiques, aes(x=dep)) + geom_bar()+ggtitle("Nombre d'accident par departement")+ xlab("Numero de departement")+ylab("Nombre d'accident")
ggplot(caracteristiques[caracteristiques$an==2019,], aes(x=as.Date(paste(an, mois, jour, sep='-')))) + geom_bar()+ggtitle("Nombre d'accident par jour en 2019")+ xlab("Date")+ylab("Nombre d'accident")
usagers <- read.csv("clean_datasets/usagers.csv", sep=',', header = TRUE)

str(usagers)
ggplotly(ggplot(usagers, aes(x=an-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre d'accident en fonction de l'age des usagees en 2019")+xlab("Age")+ylab("Nombre d'accident"))



# ================================== ==================== ================================== #
# ================================== PLOT EN CONSTRCUTION ================================== #
# ================================== ==================== ================================== #

usagers <- read.csv("usagers-2019.csv", sep=';')
str(usagers)
View(usagers)

ggplot(usagers, aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des personnes impliquées dans un accident de la route en France en 2019")

ggplot(usagers[usagers$place==1,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des conducteurs impliqués dans un accident de la route en France en 2019")

ggplot(usagers[usagers$place!=1,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des non conducteurs impliqués dans un accident de la route en France en 2019")

ggplot(usagers[usagers$place==2,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des passagé avant ou moto impliqués dans un accident de la route en France en 2019")

ggplot(usagers[usagers$place==10,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("place inconnu (10 ?)")


ggplot(usagers[usagers$catu==1,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Conducteur")
ggplot(usagers[usagers$catu==3,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Pietons")


labeller.catu <- function(variable, value){ return (list("1"="Indemne", "2"="Tué", "3"="Blessé hospitalisé", "4"="Blessé léger")[value])}
ggplot(usagers[usagers$catu==3,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(vars(grav, sexe), nrow = 4, labeller=labeller.catu ) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 10)) + ggtitle("")
ggplot(usagers[usagers$catu==3,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(vars(grav, sexe), nrow = 4) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("")















str(d1)
ggplot(d, aes(x=Date, y=Count)) + geom_line






d <- setNames(data.frame(table(usagers[usagers$place != 1,]$place)), c('place', 'count'))
plot_ly(d, x=~place) %>% add_bars(d, y=~count, text=~paste0(formatC(100 * count/sum(count), format='f', digits = 2), "%"), textposition = 'auto') %>% layout(xaxis=list(title="Place des passagers", yaxis=list(title="Nombre d'accidents")))

# TODO test en separant par le sexe plot_ly(d, x=~place) %>% add_bars(d, y=~count text=~paste0(formatC(100 * count/sum(count), format='f', digits = 2), "%"), textposition = 'auto')

d <- setNames(data.frame(table(usagers$place, usagers$sexe, usagers$an)), c('place', "sexe", "an", 'count'))
plot_ly(d, x=~place) %>% add_bars(d, y=~count, text=~sexe, textposition = 'auto') %>% layout(yaxis = list(title = 'Count'), barmode = 'group')




library(leaflet)
library(leaflet.opacity)
library(viridisLite)


map.opacity = 0.30

##### p 92 ####

paris <- caracteristiques[(caracteristiques$dep == 75) & (caracteristiques$an == 2019), ] # paris en 2019
paris <- caracteristiques[(caracteristiques$an == 2015), ] # tout les points de 2019

str(paris)

m2 <- leaflet() %>% setView(lng = 2.351462, lat = 48.8567, zoom = 12) %>%
	addTiles() %>%
	addProviderTiles("Wikimedia")

leaflet(data = reims) %>% addCircleMarkers(~long, ~lat, color="red", radius= 2, stroke = FALSE, fillOpacity = 0.5) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity))

leaflet(data = paris) %>% addCircleMarkers(~long, ~lat, color="red", radius= 2, stroke = FALSE, fillOpacity = 0.5) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity))

pal <- colorNumeric(palette = viridis(100), domain = range(paris$int))
leaflet(data = paris) %>% addCircleMarkers(~long, ~lat, color=~pal(int), radius= 3, stroke = FALSE, fillOpacity = 1) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity))
leaflet(data = paris) %>% addCircleMarkers(~long, ~lat, color=~pal(lum), radius= 3, stroke = FALSE, fillOpacity = 1) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity))
leaflet(data = paris) %>% addCircleMarkers(~long, ~lat, color=~pal(atm), radius= 3, stroke = FALSE, fillOpacity = 1) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity))
leaflet(data = paris) %>% addCircleMarkers(~long, ~lat, color=~pal(col), radius= 3, stroke = FALSE, fillOpacity = 1) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity))

caracteristiques <- read.csv("caracteristiques.csv", sep=',', header = TRUE)
reims <- caracteristiques[(caracteristiques$dep == 51) & (caracteristiques$an >= 2005), ] # reims entre 2015 et 2019
pal <- colorNumeric(palette = viridis(100), domain = range(reims$an))
leaflet(data = reims) %>% addCircleMarkers(~long, ~lat, color=~pal(an), radius= 3, stroke = FALSE, fillOpacity = 0.5) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity)) %>% addLegend("bottomright", pal = pal, values = ~an, opacity = 1) %>% setView(lng=median(reims$long, na.rm=TRUE), lat=median(reims$lat, na.rm=TRUE), zoom = 10)
pal <- colorNumeric(palette = viridis(100), domain = range(reims$hr))
leaflet(data = reims) %>% addCircleMarkers(~long, ~lat, color=~pal(hr), radius= 5, stroke = FALSE, fillOpacity = 0.75) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity)) %>% addLegend("bottomright", pal = pal, values = ~hr, opacity = 1) %>% setView(lng=median(reims$long, na.rm=TRUE), lat=median(reims$lat, na.rm=TRUE), zoom = 10)


str(caracteristiques)
outmer <- caracteristiques[(caracteristiques$dep == '974'), ] # reunion 
pal <- colorNumeric(palette = magma(100, direction = -1), domain = range(2019, 2005))
pal <- colorNumeric(palette = magma(100, direction = -1), domain = range(outmer$an))
#aberation pour les années avant 2019 la lat est positive ?????
leaflet(data = outmer) %>% addCircleMarkers(~long, ~lat, color=~pal(an), radius= 3, stroke = FALSE, fillOpacity = 5) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity)) %>% addLegend("bottomright", pal = pal, values =~an, opacity = 1)
head(read.csv("datasets/caracteristiques_2018.csv", sep=',', header = TRUE), 5)
head(read.csv("datasets/caracteristiques_2019.csv", sep=';', header = TRUE), 5)
leaflet(data = outmer) %>% addCircleMarkers(~long, ~-abs(lat), color=~pal(an), radius= 3, stroke = FALSE, fillOpacity = 5) %>% addProviderTiles("Stamen.Toner", options = list(opacity=map.opacity)) %>% addLegend("bottomright", pal = pal, values =~an, opacity = 1)
table(sort(outmer$an))
##### p 93 ####
m2 %>% addProviderTiles("Wikimedia")

##### p 94 ####
m2 %>% addProviderTiles("Esri.NatGeoWorldMap")

##### p 95 ####
m2 %>%
	addProviderTiles("Stamen.Watercolor") %>%
	addProviderTiles("Stamen.TonerHybrid")

##### p 96 ####





