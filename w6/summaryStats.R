library(dplyr)
library(tidyr)
#data <- getSeismicData(timeFrame = 'PAST_WEEK', minMagnitude = 'all')

#str(data$metadata)  %>% select(cnt = metadata$count) 

states <- tolower(unname(unlist(fromJSON('https://gist.githubusercontent.com/mshafrir/2646763/raw/8b0dbb93521f5d6889502305335104218454c2bf/states_hash.json'))))
countries <- fromJSON('https://restcountries.eu/rest/v1/all')

getSummaryInfo <- function(timeFrame = c('PAST_HOUR','PAST_DAY','PAST_WEEK','PAST_MONTH'), 
                                           minMagnitude = c('all','1.0','2.5','4.5')){
  #get time frame argument value  
  timeFrame.arg = match.arg(timeFrame)
  
  #get minimum magnitude argument value  
  minMagnitude.arg = match.arg(minMagnitude)
  
  data <- getSeismicData(timeFrame = timeFrame.arg, minMagnitude = minMagnitude.arg)
  summary <- list()
  summary$description <- data$metadata$title
  summary$num_seismic_events <- data$metadata$count 
  
  df.magnitudeByDay = data$features$properties %>% 
    transmute(magnitude=cut(mag,breaks=c(0:ceiling(max(mag,na.rm = TRUE))), na.rm=TRUE),day=round(as.POSIXct(time/1000, origin="1970-01-01"),units='days') %>% as.character) %>% group_by(day,magnitude) %>% summarise(count=n()) %>% spread(key=magnitude, value=count, fill=0) 

  df.magnitudeByCountry = data$features$properties %>% 
    transmute(magnitude=cut(mag,breaks=c(0:ceiling(max(mag,na.rm = TRUE))), na.rm=TRUE),country=place %>% strsplit(split=',') %>% sapply(., FUN = function(x){sub("^\\s+", "", x[2])}) %>% (function(x){ ifelse(tolower(x) %in% states,'USA',x) })) %>% group_by(country,magnitude) %>% summarise(count=n()) %>% spread(key=magnitude, value=count, fill = 0) 
  
  summary$magnitudeByDay <- df.magnitudeByDay
  summary$magnitudeByCountry <- df.magnitudeByCountry
  return(summary)
}


#resp <- getSummaryStats(timeFrame = 'PAST_WEEK', minMagnitude = 'all')
#resp


#https://maps.googleapis.com/maps/api/geocode/json?address=macquarie+Island&key=AIzaSyA9TqnTW5qTsssvVflVY7_A5XLnLIDKBv4  
  
