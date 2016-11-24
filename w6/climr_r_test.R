library(readr)
library(tibble)
library(magrittr)
library(dplyr)
library(tidyr)
library(climr)
library(ggplot2)
library(viridis)

#add all packages not in base package
for(pkg in c('stats','readr','tibble','magrittr','dplyr','tidyr','climr','ggplot2','viridis')){
  print(paste(pkg))
  use_package(pkg)
}

#remove all non base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

source(paste(getwd(),"R/load_clim.R", sep='/'))
source(paste(getwd(),"R/fit.climr.R", sep='/'))
source(paste(getwd(),"R/plot.climr_out.R", sep='/'))

ans1 <- load_clim('GLB')
ans2 <- climr::fit.climr(ans1, data_type = 'yearly', fit_type = 'loess')
climr::plot.climr_out(ans2)



list.files(path=paste(getwd(),'R',sep='/'))
unlink(x = paste(getwd(),'R','plot.climr_out.R',sep='/'))


library(devtools)
devtools::document()
warnings()
devtools::build()
?load_clim
?plot.climr_fit
?fit
load_all()
library(climr)
example(fit)
rm(list=ls())

ans1 = load_clim('SH')
ans2 = fit(ans1)
plot(ans2)
ans3 = fit(ans1, data_type='monthly', fit_type = 'smooth.spline')
plot(ans3)
ans4 = fit(ans1, data_type='quarterly', fit_type = 'loess')
plot(ans4)


example(plot.climr_fit)

