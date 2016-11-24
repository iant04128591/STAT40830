#function load_clim
#' Title
#'
#' @param type Either GLB, NH or SH for global, northern or southern hemishpere temp anomalies
#'
#' @return A list of \code{\link[tibble]{tibble}}s which contain the yearly , quarterly or monthyl values for each time series , as well as which series was obtained
#' @export
#'
#' @importFrom dplyr "mutate" "select" "arrange" "%>%"
#' @importFrom tidyr "gather"
#' @importFrom readr "parse_factor"
#'
#' @seealso \code{\link{fit}}, \code{\link{plot.climr_fit}}
#' @examples
#' data = load_clim(type = 'NH')
load_clim <- function(type = c('GLB','NH','SH')){

  #find type
  arg = match.arg(type)

  #build dataset url
  #arg <- 'GLB'
  url <- paste0('http://data.giss.nasa.gov/gistemp/tabledata_v3/',arg,'.Ts+dSST.csv')

  out <- read_csv(url, skip = 1, na = '***', progress = FALSE)
  #out %>% glimpse

  #sort out yearly data
  out_year = out %>%
              na.omit() %>%
              mutate(
                year = Year,
                temp = `J-D`,
                x = year) %>%
              select(year, temp, x) %>%
              arrange(x)

  #sort out monthly data
  months <- sapply(1:12, function(i){
    format(as.Date(paste0('2000-',ifelse(i < 10,'0',''),i,'-01')),'%b')
  })

  out_month <- out %>%
                gather(key = month, value = temp, Jan:Dec, na.rm = TRUE) %>%
                mutate(
                    month = parse_factor(month,levels = months, ordered = TRUE),
                    year = Year,
                    x = year + as.numeric(month)/12) %>%
                select(year, month, temp, x) %>%
                arrange(x)

  #sort out quarterly data
  quarters <-  c('DJF','MAM','JJA','SON')

  out_quarter <- out %>% gather(key = quarter,
                                value = temp,
                                DJF:SON,
                                na.rm = TRUE) %>%
                          mutate(
                            quarter = parse_factor(quarter,levels = quarters, ordered = TRUE),
                            year = Year,
                            x = year + as.numeric(quarter)/4 - 0.25
                            ) %>%
                          select(year, quarter, temp, x) %>%
                          arrange(x)

  out_list <- list(
    clim_year = out_year,
    clim_quarter = out_quarter,
    clim_month = out_month,
    type = arg
  )

  class(out_list) <- 'climr'

  return(out_list)
}
