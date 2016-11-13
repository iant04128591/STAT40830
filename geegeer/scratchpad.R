install.packages("rvest")


library(rvest)
race <- read_html("http://www.sportinglife.com/racing/racecards/13-11-2016/cheltenham/racecard/752362/sky-bet-supreme-trial-novices-hurdle-grade-2-registered-as-the-sharp-novices-hurdle")

race %>% 
  html_nodes("#racecard tbody tr .horse-dtl .name a") %>%
  html_text() %>% freduce(sub("^\\s+", " "))

lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()


gsub("^\\s+|\\s+$", "", "  ian towey  ")