library(rCharts)

plotByLocation <- function(){
  
  data <- getSeismicData(timeFrame = 'PAST_WEEK', minMagnitude = '4.5')
  data$metadata$count
  
  my_map = Leaflet$new()
  my_map$tileLayer(provider='Esri.WorldStreetMap')
  my_map$setView(c(0, 0), zoom = 2)
  for(i in 1:nrow(data$features)) {
    longtitude <-  data$features[i,]$geometry$coordinates[[1]][1]
    latitude <-  data$features[i,]$geometry$coordinates[[1]][2]
    depth <-  data$features[i,]$geometry$coordinates[[1]][3]
    
    # my_map$addCircleMarkers(data=cities.meta, radius = ~sqrt(10*PopClass),
    #                  color = ~palfun(PopClass), stroke=FALSE, fillOpacity=0.5, layerId = ~Location)
        
    # my_map$addCircleMarkers(data=c(latitude,longtitude), radius=data$features[i,]$properties$mag)
    
      my_map$marker(c(latitude, longtitude),
                  bindPopup = paste(
                    "<ul>",
                    "<li>Place:",data$features[i,]$properties$place,"</li>",
                    "<li>Magnitude:",data$features[i,]$properties$mag,"</li>",
                    "<li>Time:",as.POSIXct(data$features[i,]$properties$time/1000, origin="1970-01-01"),"</li>",
                    "<li>Depth:",depth,"</li>",
                    "<li><a href=",data$features[i,]$properties$url,">More Details...</></li>",
                    "</ul>"))
  }
  my_map
  
}



library(rCharts)
library(rjson)
TransformDate <- function(x){
  as.numeric(as.POSIXct(x, origin="1970-01-01")) * 1000
}

x <- TransformDate(c('2013-01-01 11:05:35', '2013-03-03 04:50:35', '2013-05-05 21:09:37', '2013-07-07 12:49:05'))
y <- c(1,56,123,1000)

w<-TransformDate(c('2013-01-10 11:05:35', '2013-03-13 04:50:35', '2013-05-15 21:09:37', '2013-07-17 12:49:05'))
z<-c(10, 100, 70, 500)

df1 <- data.frame(x = x,y = y)
df2 <- data.frame(x = w, y = z)
combo <- rCharts:::Highcharts$new()
combo$series(list(list(data = rCharts::toJSONArray2(df1, json = F, names = F), name = "Temp1", marker = list(fillColor = c('#999'), lineWidth=6, lineColor=c('#999'))),
                  list(data = rCharts::toJSONArray2(df2, json = F, names = F), name = "Temp2")))


combo$xAxis(type='datetime')
combo$chart(type = "scatter")
combo$chart(zoomType="x")

combo


map3 = Leaflet$new()
map3$setView(c(41.7627, -72.6743), zoom = 13)
map3$marker(c(41.7627, -72.6743), bindPopup = "<p> Hi. I am a popup </p>")
map3$marker(c(41.70, -72.60), bindPopup = "<p> Hi. I am another popup </p>")
map3$print(include_assets=T)
map3


# my_map$addCircleMarkers(data=cities.meta, radius = ~sqrt(10*PopClass),
#                  color = ~palfun(PopClass), stroke=FALSE, fillOpacity=0.5, layerId = ~Location)

data$features %>% nrow

dat = data.frame(
  lon=sapply(data$features$geometry$coordinates,FUN = function(p){
    p[1]
  }),
  lat=sapply(data$features$geometry$coordinates,FUN = function(p){
    p[2]
  })
)

require(rCharts)
# dat <- read.csv('~/Downloads/ToUseWithLeaflet.csv', stringsAsFactors = F)
dat_list <- toJSONArray2(dat, json = F)

L1 <- Leaflet$new()
L1$setView(c(0, 0), zoom = 2)
L1$geoJson(toGeoJSON(dat_list, lat = 'lat', lon = 'lon'),
           onEachFeature = '#! function(feature, layer){
    layer.bindPopup(feature.properties.label)
 } !#',
           pointToLayer =  "#! function(feature, latlng){
    return L.circleMarker(latlng, {
      radius: 8,
      fillColor: feature.properties.Color || 'red',    
      color: '#000',
      weight: 1,
      fillOpacity: 0.8
    })
 } !#"         
)
L1

dat_list[[1]]$lontitude


c(5,10) %>% .[2]