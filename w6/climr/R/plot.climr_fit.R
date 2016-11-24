#' Title
#'
#' @param Output from the \code{\link{fit}} function
#' @param time_grid An Optional time grid over which to produce fitted values of the model
#' @param ... Other arguments to plot,(not currently implemented)
#'
#' @return Nothing: just a nice plot
#' @seealso \code{\link{load_clim}},  \code{\link{fit}}
#' @export
#' @import ggplot2
#' @importFrom tibble "tibble"
#' @importFrom viridis "scale_color_viridis"
#'
#' @examples
#' ans1 = load_clim('SH')
#' ans2 = fit(ans1)
#' plot(ans2)
#' ans3 = fit(ans1, data_type='monthly', fit_type = 'smooth.spline')
#' plot(ans3)
#' ans4 = fit(ans1, data_type='quarterly', fit_type = 'loess')
#' plot(ans4)

#' @export
plot.climr_fit = function(x, time_grid = pretty(x$data$x, n = 100), ...){

  # create a nice plot from the output of fit.climr

  #get data
  df = x$data

  #get some predicted values based on the time grid
  if(x$fit_type == 'lm'){
    fits = tibble(time_grid, pred = predict(x$model, newdata = tibble(time_grid)))
  } else if  (x$fit_type == 'loess'){
    fits = tibble(time_grid, pred = predict(x$model, newdata = tibble(x = time_grid))) %>% na.omit()
  } else if  (x$fit_type == 'smooth.spline'){
    fits = tibble(time_grid, pred = predict(x$model, tibble(time_grid)))$y[,1] %>% na.omit()
  }

  #Finally create plot
  ggplot(df, aes(x,temp)) +
    geom_point(aes(colour = temp)) +
    theme_bw() +
    xlab('Year') +
    ylab('Temparature anomaly') +
    geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
    theme(legend.position = 'None') +
    scale_color_viridis()
}
