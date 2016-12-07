
# install.packages('jsonlite')
# library(jsonlite)
# library(magrittr)

#function getSeismicData
#' Title
#'
#' @param timeFrame One of: PAST_HOUR, PAST_DAY, PAST_WEEK, PAST_MONTH
#'
#' @return A list of \code{\link[tibble]{tibble}}s which contain the yearly , quarterly or monthyl values for each time series , as well as which series was obtained
#' @export
#'
#' @importFrom base "gsub" "match.arg" "tolower" "paste0" "paste0" "tryCatch" "print" "class"
#' @importFrom magrittr "%>%"
#' @importFrom jsonlite "fromJSON"
#'
#' @seealso \code{\link{TODO}}, \code{\link{TODO}}
#' @examples
#' data <- getSeismicData(timeframe = 'PAST_DAY', min_magnitude = 'all')
#' data <- getSeismicData(timeframe = 'PAST_WEEK', min_magnitude = '1')
#' data <- getSeismicData(timeframe = 'PAST_MONTH', min_magnitude = '2.5')
#
getSeismicData <- function(timeFrame = c('PAST_HOUR','PAST_DAY','PAST_WEEK','PAST_MONTH'), 
                           minMagnitude = c('all','1.0','2.5','4.5')){
  #get time frame argument value  
  timeFrame.arg = match.arg(timeFrame)

  #get minimum magnitude argument value  
  minMagnitude.arg = match.arg(minMagnitude)
  
  #
  timeFrame.arg.normalized = timeFrame.arg %>% gsub(pattern='PAST_',replacement='',x=.) %>% tolower
  
  #build url
  url = paste0('http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/',minMagnitude.arg,'_',timeFrame.arg.normalized,'.geojson')

  #fetch geojsin from url
  seismic.geojson = tryCatch({ 
    fromJSON(url)},warning = function(w) {
      print(paste("WARNING",w))
    }, error = function(e) {
      print(paste("ERROR",e, url))
    }, finally = {
      NULL
    })

  class(seismic.geojson) <- 'geojson'    
  return(seismic.geojson)
}


# data <- getSeismicData(timeFrame = 'PAST_WEEK', minMagnitude = 'all')
# class(data)
# data$metadata$count
# 
# data <- getSeismicData(timeFrame = 'PAST_WEEK', minMagnitude = '1')
# data$metadata$count
# 
# data <- getSeismicData(timeFrame = 'PAST_DAY', minMagnitude = '2.5')
# data$metadata$count


