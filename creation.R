install.packages(c('ggplot2', 'dplyr', 'lubridate', 'plotly'))

library("ggplot2")
library("dplyr")
library("lubridate")
library("plotly")
library(leaflet)
library(leaflet.opacity)
library(viridisLite)
library(broom)
# ================================== ======= ================================== #
# ================================== PLOT OK ================================== #
# ================================== ======= ================================== #

accidents <- read.csv("clean_datasets/accidents.csv", sep=',', header = TRUE)
str(accidents)

anhr <- setNames(data.frame(table(accidents$an, accidents$hr)), c("an", "hr", "count"))
str(anhr)

plot_ly(anhr, x=~hr, y=~count, color=~an, type='scatter', mode = 'lines')%>% layout(title = "Nombre d'accident en fonction de l'heure de l'annee de 2005 a 2019",xaxis = list(title="Heure"), yaxis = list(title="Nombre d'accident"))
plot_ly(anhr, x=~an, y=~count, color=~hr, type='scatter', mode = 'lines')%>% layout(title = "Nombre d'accident en fonction de l'heure de l'annee de 2005 a 2019",xaxis = list(title="Annee"), yaxis = list(title="Nombre d'accident"))
plot_ly(anhr, x=~hr, y=~an, z=~count, type="heatmap")%>% layout(title = "Nombre d'accident en fonction de l'heure de l'annee de 2005 a 2019",xaxis = list(title="Heure"), yaxis = list(title="Annee"))
plot_ly(anhr, x=~hr, y=~an, z=~count, color=~an, type="scatter3d",  mode = 'lines', line = list(width = 30)) # osef de lui ?

plot_ly(anhr, x=~hr, y=~count, color=~an, type="bar")

# Test l'heure des accidents en fonction des jours de la semaine
d5 <- setNames(data.frame(table(weekdays(as.Date(paste(accidents$an, accidents$mois, accidents$jour, sep='-'))), accidents$hr )),c("Day", "hr", "Count"))

plot_ly(d5, x=~Day, y=~Count, color=~hr, type="bar") %>% layout(yaxis = list(title="Nombre d'accidents"), xaxis = list(title = "Jours de la semaine",categoryorder = "array", categoryarray = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")))
plot_ly(d5, x=~hr, y=~Count, color=~factor(Day , levels=c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")), type="bar")

d1 <- setNames(data.frame(table(as.Date(paste(accidents$an, accidents$mois, accidents$jour, sep='-')))),c("Date","Count"))
d1$day <- weekdays(as.Date(d1$Date))
dim(d1)[1]
str(d1)

d6 <- NULL
d6 <- d1[,c("Date", "Count")]
d6$breaks <- as.Date(cut(as.Date(d1$Date, format = "%Y-%m-%d"), breaks = "33 days"), format = "%Y-%m-%d")
d6 <- d6 %>% group_by(breaks) %>% summarise(Count = sum(Count))
str(d6)

lois = data.frame(Date = c("2012-01-05", "2012-07-01", "2018-07-01"), text=c("avertisseurs de radars interdits", "éthylotest obligatoirs", "loi 80Km/h"), colors="red", "black", "green")

l <- loess(Count ~ as.numeric(breaks), data=d6)
r <- setNames(augment(l), c("Count", ".se.fit", ".fitted"))
r$breaks <- d6$breaks
plot_ly(d6, x=~breaks) %>%
	add_lines(y=~Count, name='Total', opacity=0.25, line = list(color = '#553333')) %>%
	add_lines(y=~fitted(l), line=list(color='#07A4B5'), name="Loess Smoother", showlegend=TRUE) %>%
	add_ribbons(data=r, ymin = ~.fitted - 0.04 * .se.fit, ymax = ~.fitted + 0.04 * .se.fit, line = list(color = 'rgba(7, 164, 181, 0.05)'), fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Standard Error") %>%
	add_segments(data=lois, x=~Date, xend=~Date, color=~text, colors=~colors, text=~text, y=min(d6$Count), yend=max(d6$Count))


# ================================== ==================== ================================== #
# ================================== PLOT EN CONSTRCUTION ================================== #
# ================================== ==================== ================================== #

usagers <- read.csv("clean_datasets/usagers.csv", sep=',')
str(usagers)
View(usagers)

ggplot(usagers, aes(x=an-an_nais)) + geom_bar() + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des personnes impliquées dans un accident de la route en France en 2019")

labeller.sexe <- function(variable, value){ return (list("1"="Homme", "2"="Femme")[value])}
ggplot(usagers,                   aes(x=an-an_nais)) + geom_bar() + facet_wrap(~sexe, labeller=labeller.sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des personnes impliquées dans un accident de la route en France en 2019")
ggplot(usagers[usagers$catu==1,], aes(x=an-an_nais)) + geom_bar() + facet_wrap(~sexe, labeller=labeller.sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des conducteurs impliqués dans un accident de la route en France en 2019")
ggplot(usagers[usagers$catu==2,], aes(x=an-an_nais)) + geom_bar() + facet_wrap(~sexe, labeller=labeller.sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des non conducteurs impliqués dans un accident de la route en France en 2019")



d7 <- setNames(data.frame(table(usagers$an - usagers$an_nais, usagers$sexe, usagers$grav)), c("age", "sexe", "grav", "total"))
ggplotly(ggplot(d7) + aes(x=age, y=total, fill=factor(grav, levels=c(1,4,3,2))) + geom_bar(stat="identity") + scale_x_discrete(name="âge", breaks = seq(0, 100, 5)))
ggplotly(ggplot(d7) + aes(x=age, y=total, fill=factor(grav, levels=c(1,4,3,2))) + geom_bar(stat="identity") + facet_wrap(~sexe, labeller=labeller.sexe) + scale_x_discrete(name="âge", breaks = seq(0, 100, 5)))




ggplot(usagers[usagers$place==2,], aes(x=an-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des passagé avant ou moto impliqués dans un accident de la route en France en 2019")

ggplot(usagers[usagers$place==10,], aes(x=an-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("place inconnu (10 ?)")


ggplot(usagers[usagers$catu==1,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Conducteur")
ggplot(usagers[usagers$catu==3,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Pietons")


labeller.catu <- function(variable, value){ return (list("1"="Indemne", "2"="Tué", "3"="Blessé hospitalisé", "4"="Blessé léger")[value])}
ggplot(usagers[usagers$catu==3,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(vars(grav, sexe), nrow = 4, labeller=labeller.catu ) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 10)) + ggtitle("")
ggplot(usagers[usagers$catu==3,], aes(x=2019-an_nais)) + geom_bar() + facet_wrap(vars(grav, sexe), nrow = 4) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("")













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

ggplotly(ggplot(d2, aes(x=Date, y=Count)) + geom_line() + geom_smooth(formula=y~x, method = 'loess'))
plot_ly(d3, x=~Date) %>% add_lines(y=~Count)

plot_ly(d1, x=~day) %>% add_boxplot(y=~Count) %>% layout(xaxis = list(categoryorder = "array", categoryarray = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")))



plot_ly(d1, x=~factor(month.abb[month(Date)], levels = month.abb)) %>% add_boxplot(y=~Count, color=~factor(day , levels=c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))) %>% layout(boxmode = "group")
plot_ly(d1, x=~day) %>% add_boxplot(y=~Count, color=~factor(month.abb[month(Date)], levels = month.abb)) %>% layout(boxmode = "group")

plot_ly(d1, x=~day(Date)) %>% add_boxplot(y=~Count)

d4 <- d1 %>%
	mutate(Mois=month(Date), Day=day(Date), p=paste(month(Date), day(Date)), Date = as.Date(paste(year(Date), month(Date), day(Date), sep = '-'))) %>%
	group_by(Day, Mois, p, Date) %>%
	summarise(Count = sum(Count))
d4 <- d4[ with(d4, order(Mois, Day)), ]

plot_ly(d2, x=~factor(month.abb[Mois], levels = month.abb)) %>% add_boxplot(y=~Count) #par mois
plot_ly(d1, x=~week(Date)) %>% add_boxplot(y=~Count) #par semaine
plot_ly(d4, x=~p) %>% add_boxplot(y=~Count) %>% layout(xaxis = list(categoryorder = "array", categoryarray =d4$p)) #par jour

ggplot(caracteristiques, aes(x=dep)) + geom_bar()

str(d1)
ggplot(d, aes(x=Date, y=Count)) + geom_line()
ggplot(caracteristiques, aes(x=an)) + geom_bar()
ggplot(caracteristiques, aes(x=mois)) + geom_bar()
ggplot(caracteristiques, aes(x=jour)) + geom_bar()
ggplot(caracteristiques, aes(x=dep)) + geom_bar()

ggplot(caracteristiques[caracteristiques$an==2019,], aes(x=as.Date(paste(an, mois, jour, sep='-')))) + geom_bar()

usagers <- read.csv("clean_datasets/usagers.csv", sep=',', header = TRUE)

str(usagers)
ggplotly(ggplot(usagers, aes(x=an-an_nais)) + geom_bar() + facet_wrap(~sexe) + scale_x_continuous(name="âge", limits=c(0, 100), breaks = seq(0, 100, 5)) + ggtitle("Nombre et âge des personnes impliquées dans un accident de la route en France en 2019"))

d <- setNames(data.frame(table(usagers[usagers$place != 1,]$place)), c('place', 'count'))
plot_ly(d, x=~place) %>% add_bars(d, y=~count, text=~paste0(formatC(100 * count/sum(count), format='f', digits = 2), "%"), textposition = 'auto')

# TODO test en separant par le sexe plot_ly(d, x=~place) %>% add_bars(d, y=~count text=~paste0(formatC(100 * count/sum(count), format='f', digits = 2), "%"), textposition = 'auto')

d <- setNames(data.frame(table(usagers$place, usagers$sexe, usagers$an)), c('place', "sexe", "an", 'count'))
plot_ly(d, x=~place) %>% add_bars(d, y=~count, text=~sexe, textposition = 'auto') %>% layout(yaxis = list(title = 'Count'), barmode = 'group')







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





