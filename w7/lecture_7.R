# Interactive visualisations and extensions to ggplot2

# Lattice graphics - http://lattice.r-forge.r-project.org/Vignettes/src/lattice-intro/lattice-intro.pdf horrible. We will not cover this

# ggplot2 extensions
# ggmap - https://github.com/dkahle/ggmap
# gganimate - https://github.com/dgrtwo/gganimate a bit weak
# ggraph - https://github.com/thomasp85/ggraph cool but not really finished
# ggalt - https://github.com/hrbrmstr/ggalt - needs proj4 which is a pain to install

# Interactive graphics

# rCharts - https://github.com/ramnathv/rCharts
# ggiraph - https://cran.r-project.org/web/packages/ggiraph/vignettes/ggiraph.html
# Also (not covered plotly)

# A quick note about installing packages
# Lots of them are available on Github (later lecture). You can install a package from github with
# library(devtools)
# install_github('user_name/pkg_name')
# Lots of the cutting edge packages are here

# ggplot2 extensions 1 - ggmap --------------------------------------------

#install.packages('ggmap')
#devtools::install_github('dkahle/ggmap') - need the devtools version more stable
library(ggmap)

# Createa a simple map of Ireland - specify long/lat ranges
ei = c(left = -11, bottom = 51, right = -5, top = 56)
# Get the map. zoom controls the level of detail
# Will download maps on the fly
map = get_stamenmap(ei, zoom = 7, maptype = "toner-lite")
ggmap(map, extent = 'device') # Bit ugly

# Can change the maptype but beware not all types are supported at each zoom level
# See ?get_stamenmap
map = get_stamenmap(ei, zoom = 6, maptype = "terrain")
ggmap(map, extent = 'device') # Nicer

# Or something more artistic
map = get_stamenmap(ei, zoom = 8, maptype = "watercolor")
ggmap(map, extent = 'device') # Beautiful

# Can do all of Europe and use pipes
europe = c(left = -12, bottom = 35, right = 30, top = 63)
library(dplyr)
get_stamenmap(europe, zoom = 5) %>% ggmap(extent = 'device')

# Don't like this style, use Google maps instead
?get_googlemap("Ireland", zoom = 7) %>% ggmap(extent = 'device')
# Can put in anything in the first argument, then just adjust the zoom
get_googlemap("Kilimanjaro", zoom = 8, maptype = 'hybrid') %>% ggmap(extent = 'device')

## EXERCISE 1 - get some maps

# Write get_googlemap commands in the form:
# get_googlemap(location, zoom, maptype, ...) %>% ggmap(extent = 'device').
# for the following situations:
# a) A satellite map€ of the Eiffel Tower closely at zoom 18
?get_googlemap( "Eiffel Tower", zoom = 18, maptype = 'satellite') %>% ggmap(extent = 'device')
get_googlemap( "Eiffel Tower", zoom = 18, maptype = 'satellite') %>% ggmap(extent = 'device')
# b) A road map of Los Angeles at zoom 14
get_googlemap("Los Angeles", zoom = 14, maptype = 'roadmap') %>% ggmap(extent = 'device')
# c) A black and white map of the terrain of Nepal at zoom level 6
get_googlemap("Nepal", zoom = 6, color = "bw", maptype = 'terrain') %>% ggmap(extent = 'device')

# Adding layers to maps ---------------------------------------------------

# The fun starts when you add stuff to the plot
# crime data comes with ggmap
str(crime)
head(crime)
# Plot the locations of violent crimes
library(readr)
violent_crimes = crime %>%
  filter(offense != "auto theft",
         offense != "theft",
         offense != "burglary") %>%
  mutate(offense = parse_factor(offense,
                                levels = c("robbery", "aggravated assault", "rape", "murder"))) %>%
  filter(-95.39681 <= lon & lon <= -95.34188,
         29.73631 <= lat & lat <=  29.78400)

str(violent_crimes)
# Create the area and get the map
area = c(left = -95.39615, bottom = 29.73646, right = -95.34190, top = 29.78391)
map = get_stamenmap(area, zoom = 14, maptype = "toner-lite")

# Now plot, just replace the ggplot call with ggmap
ggmap(map) + geom_point(data = violent_crimes, aes(x = lon, y = lat, colour = day))

# Make fancier
ggmap(map) + geom_density2d(data = violent_crimes, aes(x = lon, y = lat, colour = ..level..))
# Or try hexagonal
ggmap(map) + coord_cartesian() +
  geom_hex(data = violent_crimes, aes(x = lon, y = lat), bins = 20)

# Or even fancier - highlight robberies
library(viridis)
robberies = violent_crimes %>% filter(offense == "robbery")
map = get_stamenmap(area, zoom = 15, maptype = "toner-background")
ggmap(map) + stat_density_2d(data = robberies,
                             aes(x = lon, y = lat, fill = ..level..),
                             geom = 'polygon',
                             alpha = 0.5,
                             colour = NA) +
  scale_fill_viridis(option = 'B')

## EXERCISE 2

# Create a data frame that contains only the auto thefts. 
# Then create a plot like the above to show the density 
# of the crimes. Add a facet_wrap by the variable hour to 
# see how the crimes vary by hour.
# Which hour has the clear peak in auto thefts?

auto_theft = crime %>% filter(offense == "auto theft") 
max(auto_theft$hour)
area = c(left = -95.39615, bottom = 29.73646, right = -95.34190, top = 29.78391)
str(auto_theft)
map = get_stamenmap(area, zoom = 15, maptype = "toner-background")
ggmap(map) + 
  geom_density2d(data = auto_theft, aes(x = lon, y = lat, colour = ..level..)) + 
  facet_wrap(~hour)

# gganimate ---------------------------------------------------------------

# Combines the animate package with ggplot2
# Very simple to use - just add in a frame command
# devtools::install_github('dgrtwo/gganimate')

# Use the gapminder data
# See https://www.youtube.com/watch?v=jbkSRLYSojo in case you haven't already seen it
library(gapminder) # install.packages("gapminder")
str(gapminder)
library(scales)
library(ggplot2)
p = ggplot(gapminder, aes(x = gdpPercap,
                          y = lifeExp,
                          size = pop,
                          color = continent,
                          frame = year)) + # This is the new bit
  geom_point() +
  scale_x_log10(labels = comma_format()) +
  theme_bw() +
  xlab('GDP per capita') +
  ylab('Life expectancy')

# Now
library(gganimate)
gg_animate(p)
# Had to install imageMagick
# For Mac from http://cactuslab.com/imagemagick/
# For Windows http://www.imagemagick.org/script/binary-releases.php#windows

# Save in a load of different formats
# gg_animate(p, "output.gif")
# gg_animate(p, "output.mp4")
# gg_animate(p, "output.swf")
# gg_animate(p, "output.html")

# You can add 'frame' anywhere you like

# You can also use cumulative if you don't want previous points to disappear
p2 = ggplot(gapminder,
            aes(x = gdpPercap,
                y = lifeExp,
                frame = year,
                colour = continent)) +
  geom_path(aes(cumulative = TRUE,
                group = country)) +
  scale_x_log10(labels = comma_format()) +
  theme_bw() +
  xlab('GDP per capita') +
  ylab('Life expectancy') +
  facet_wrap(~ continent)

gg_animate(p2)
gg_animate(p2,
           ani.width = 800,
           ani.height = 400,
           interval = 0.5,
           filename = "output.html") # Gives you some controls too

# Or even add in an animated smooth
p3 = ggplot(gapminder, aes(x = gdpPercap,
                           y = lifeExp,
                           frame = year,
                           colour = continent)) +
  geom_path(aes(cumulative = TRUE,
                group = country)) +
  geom_smooth(aes(group = year),
              method = "loess",
              show.legend = FALSE) +
  scale_x_log10(labels = comma_format()) +
  facet_wrap(~continent)

gg_animate(p3)

## EXERCISE 3

# See if you can combine the gganimate and ggmap packages to create an animated 24 hour map of violent crimes using the hour variable
# Start with the point version of the data
library(ggmap)
library(dplyr)
library(readr)
library(ggplot2)

violent_crimes = crime %>%
  filter(offense != "auto theft",
         offense != "theft",
         offense != "burglary") %>%
  mutate(offense = parse_factor(offense,
                                levels = c("robbery", "aggravated assault", "rape", "murder"))) %>%
  filter(-95.39681 <= lon & lon <= -95.34188,
         29.73631 <= lat & lat <=  29.78400)


GeomRasterAnn
str(violent_crimes)
# Create the area and get the map
area = c(left = -95.39615, bottom = 29.73646, right = -95.34190, top = 29.78391)
map = get_stamenmap(area, zoom = 14, maptype = "toner-lite")

p = ggmap(map) + geom_point(data = violent_crimes, aes(x = lon, y = lat, colour = day, frame = day))
gg_animate(p)

# Adjust the geom_point call to add an animation for offense (and colour by offense).
# Which of the offenses has the smallest number of points?

# Animate and colour it again this type by month (hint: you will need to convert it into a non-ordered factor)
# Which month has the highest number of violent crimes: January, March, May, or August?

# rCharts -----------------------------------------------------------------

# Install with
#install_github('ramnathv/rCharts')
library(rCharts)
# Heard of problems installing this on Windows. See how you get on
# This is a very early-days rough and ready package

# Lots of different fancy chart styles here, all based on d3.js
# GitHub page is ok, help files pretty useless

# The main function is rPlot - uses a formula interface
# Example using the gapminder data
rPlot(lifeExp ~ year | continent,
      data = gapminder,
      color = 'country',
      type = 'point')

# Example using the titanic data
ti = tbl_df(Titanic)
rPlot(n ~ Class | Survived + Age,
      color = 'Survived',
      data = ti,
      type = 'bar')


# Next one is mPlot for morris plot - time series line graphs
library(tidyr)
gap_2 = gapminder %>% filter(country == 'Tanzania' |
                             country == 'Ireland' |
                             country == 'China') %>%
  select(country, year, lifeExp) %>%
  spread(key = country,
         value = lifeExp) %>%
  mutate(year_2 = paste0(year,'-01-01'))

m1 = mPlot(x = 'year_2',
           y = c('Ireland', 'Tanzania', 'China'),
           type = 'Line',
           data = gap_2)
m1$set(pointSize = 0, lineWidth = 1)
m1

# Interactive maps
my_map = Leaflet$new()
my_map$setView(c(29.76, -95.36), zoom = 7)
for(i in 1:nrow(robberies)) {
  my_map$marker(c(robberies$lat[i], robberies$lon[i]),
                bindPopup = paste("<p>", robberies$location[i], "</p>"))
}
my_map

# Sharing plots

# Want to publish this?
my_map$publish('Robberies map', host = 'rpubs')
# Need to create an rpubs account

# No exercises here in case of installation difficulties

# ggiraph -----------------------------------------------------------------

# At last, a proper R package
#install.packages('ggiraph')
# or devtools::install_github('davidgohel/ggiraph')
library(ggiraph)

# Recall the first plot we created in lecture 3
p = ggplot(mpg, aes(x = displ, y = hwy, colour = as.factor(cyl))) +
  xlab('Engine size') +
  ylab('Highway miles per gallon') +
  stat_smooth() +
  scale_color_discrete(name="Number of\ncylinders")

# The standard plot was
p + geom_point()

# Simply change geom_point to geom_point interactive and add in a tool tip
p2 = p + geom_point_interactive(aes(tooltip = model),
                                size = 2)
ggiraph(code = print(p2), width = 0.65)
# Click on open in browser icon to get full picture

# Make fancier - colours the points when you hover
p3 = p + geom_point_interactive(aes(tooltip = model,
                                    data_id = model),
                                size = 2)
ggiraph(code = print(p3), width = 0.65)

# A cool example from the vignette
# Convert everything to lower case (why?)
crimes = data.frame(state = tolower(rownames(USArrests)), USArrests)
# create an 'onclick' column - window.open is javascript
crimes$onclick = sprintf("window.open(\"%s%s\")",
                         "http://en.wikipedia.org/wiki/",
                         as.character(crimes$state))

gg_crime = ggplot(crimes,
                  aes(x = Murder,
                      y = Assault,
                      color = UrbanPop)) +
  geom_point_interactive(aes(data_id = state,
                             tooltip = state,
                             onclick = onclick),
                         size = 3) +
  scale_color_viridis() +
  theme_bw()

ggiraph(code = print(gg_crime),
        hover_css = "fill-opacity:.3;cursor:pointer;",
        width = 1)


# Full list of new geoms
# geom_bar_interactive
# geom_point_interactive
# geom_line_interactive
# geom_polygon_interactive
# geom_map_interactive
# geom_path_interactive
# geom_rect_interactive
# geom_segment_interactive
# geom_text_interactive
# geom_boxplot_interactive
# All allow tooltips, onclicks and data_id

# Possible to export these into Shiny apps (next week)

## EXERCISE 4

# Go back to lecture 3 and the two examples we created. For the first example (global temperature time series) use geom_line_interactive on the first geom_line (the data mean value) to create a plot where hovering over the line will provide the data value. Fill in the blanks below for the missing line:
# geom_line_interactive(size = 1, aes(tooltip = [A], data_id = [A]))

# For the second example add in geom_map_interactive and update so that the value (e.g. the number of arrests) on the graph is highlighted when the mouse hovers over the states. Fill in the blanks below:
# geom_map_interactive(aes(fill = [A], data_id = [A], tooltip = [A]), map = states_map)




