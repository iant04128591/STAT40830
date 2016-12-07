#clear r env
rm(list = ls())

#inport required library
library(ggplot2)
library(reshape2)
library(latex2exp)


url <- "http://data.giss.nasa.gov/gistemp/graphs/graph_data/Monthly_Mean_Global_Surface_Temperature/graph.csv"
#Read data to data frame
url_data = read.csv(url,skip = 1,colClasses = 'numeric')
# Get data since 1996 to present
url_data_1996_PRESENT = subset(url_data, Year.Month > 1996)
## 'Melt' to long format
url_data_1996_PRESENT__MELT = melt(url_data_1996_PRESENT, id = c('Year.Month')) 
## Change factor labels to suit legend
levels(url_data_1996_PRESENT__MELT$variable) <- c('Meteorological Stations', 'Land-Ocean Temperature Index')

## Plot 
ggplot(url_data_1996_PRESENT__MELT, aes(x = Year.Month, y = value, colour= variable)) + 
  geom_line(size=0.25, aes(linetype=variable)) +
  geom_point(aes(shape=variable)) + 
  ylab(TeX('Temperature Anomaly in (C)'))   + 
  xlab("") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), limits = c(1995,2020),expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(0.2, 1.8, by = 0.2), limits = c(0.2,1.8),expand = c(0,0)) + 
  ggtitle(TeX('Monthly Mean Global Surface Temperature')) +
  theme(panel.border = element_blank(),
        axis.title.y = element_text(size = rel(1.4)),
        axis.line.x = element_line(size = 0.25, linetype = "solid", colour = "black", lineend = "butt"),
        axis.line.y = element_line(size = 0.25, linetype = "solid", colour = "black", lineend = "butt"),
        axis.text.x = element_text(size=14,margin(1)),
        axis.text.y = element_text(size=14,margin(1)),
        legend.title = element_blank(),
        legend.position = c(0.21,0.93),
        legend.background = element_rect(colour = "black" ),
        legend.key = element_blank(),
        legend.text=element_text(size=14),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,1,0,0),"cm"),
        axis.ticks.length = unit(0.25,"cm")
  ) +
  guides(colour = guide_legend(keywidth = 3, keyheight = 1)) +
  scale_color_manual(values=c('black','red')) +
  scale_shape_manual(values=c(15,15)) +
  scale_linetype_manual(values = c(3,1)) +
  annotate(geom="text", x=2017.5, y=0.3, label="NASA GISS", color="black", size=6)
