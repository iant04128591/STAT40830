library(dplyr)

data <- getSeismicData(timeFrame = 'PAST_WEEK', minMagnitude = 'all')

str(data$metadata)  %>% select(cnt = metadata$count) 


getSummaryStats <- function(timeFrame = c('PAST_HOUR','PAST_DAY','PAST_WEEK','PAST_MONTH'), 
                                           minMagnitude = c('all','1.0','2.5','4.5')){
  data <- getSeismicData(timeFrame = 'PAST_MONTH', minMagnitude = 'all')
  summary <- list()
  summary$description <- data$metadata$title
  summary$frequency <- data$metadata$count 
  
  data$features$properties %>% transmute(magnitude=mag,day=round(as.POSIXct(time/1000, origin="1970-01-01"),units='days') %>% as.character) %>% group_by(day)

  df <- data$features$properties %>% 
    transmute(magnitude=cut(mag,breaks=c(0:ceiling(max(mag,na.rm = TRUE))), na.rm=TRUE),day=round(as.POSIXct(time/1000, origin="1970-01-01"),units='days') %>% as.character) %>% group_by(day,magnitude) %>% summarise(count=n()) %>% spread(key=magnitude, value=count) 

  df$total <- round(apply(df[,-1],1, 'sum',na.rm = TRUE),0)
  df$avg <- round(apply(df[,-1],1, 'mean',na.rm = TRUE),0)
  df[is.na(df)] <- 0
  return(summary)
}



rownames(df) <- df$day

?rowSums

?mean

round(as.POSIXct(data$features$properties$time/1000, origin="1970-01-01"),units='days')  %>% as.character








?group_by

getSummaryStats(timeFrame = 'PAST_HOUR', minMagnitude = 'all')


by_cyl <- group_by(mtcars, cyl)

by_cyl$