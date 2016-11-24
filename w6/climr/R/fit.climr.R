#' Fit basic statistical moels to climate data
#'
#' @param obj An object of class \code{climr} from \code{\link{load_clim}}
#' @param data_type The type of data to be analysed , either yearly, monthly or quarterly
#' @param fit_type The type of model required , eitheither linear regression (\code{lm}), loess or smoothing spline (\code{smooth.spline})
#'
#' @return Return a list of class \code{climr_fit} which includes the model details as well as the data set and the fit type used
#' @seealso \code{\link{load_clim}},  \code{\link{plot.climr_fit}}
#' @export
#' @importFrom magrittr "extract2" "%$%"
#' @importFrom stats "lm" "loess" "smooth.spline" "na.omit" "predict"
#'
#' @examples
#' ans1 = load_clim('SH')
#' ans2 = fit(ans1)
#' ans3 = fit(ans1, data_type='monthly', fit_type = 'smooth.spline')
#' ans4 = fit(ans1, data_type='quarterly', fit_type = 'loess')
fit = function(obj,
               data_type = c('yearly', 'quarterly', 'monthly'),
               fit_type = c('lm', 'loess', 'smooth.spline')){
  UseMethod('fit')
}

#' @export
fit.climr = function(obj,
                     data_type = c('yearly', 'quarterly', 'monthly'),
                     fit_type = c('lm', 'loess', 'smooth.spline')){
  #fund out which data set
  fit_dat = match.arg(data_type)
  #find fittig method
  fit_arg = match.arg(fit_type)

  #find out which bit of data
  dat_choose = switch(fit_dat, yearly = 1, quarterly = 2, monthly = 3)

  #Get the dataset to use
  curr_dat = obj %>% extract2(dat_choose)

  #fit some models
  if(fit_arg == 'lm'){
    mod = curr_dat %$% lm(temp ~ x)
  } else if(fit_arg == 'loess'){
    mod = curr_dat %$% loess(temp ~ x)
  } else if(fit_arg == 'smooth.spline'){
    mod = curr_dat %$% smooth.spline(x, temp)
  }

  print(mod)

  out = list(model = mod, data = curr_dat, dat_type = fit_dat, fit_type = fit_arg)
  class(out) = 'climr_fit'
  invisible(out)
}
