library(shiny)
library(shinydashboard)
library(DT)
library(fs)
library(wbstats)
library(leaflet)
library(plotly)
library(tidyverse)
library(knitr)
library(kableExtra)


data_con<-read.csv("C:/Users/ArulSamy/Downloads/COVID-19-master/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
data_dec<-read.csv("C:/Users/ArulSamy/Downloads/COVID-19-master/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data_rec<-read.csv("C:/Users/ArulSamy/Downloads/COVID-19-master/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
##View(data_con)
current_date=as.Date(names(data_con)[ncol(data_con)],format = '%m/%d/%y')


data_con_sub<-data_con %>%
  pivot_longer(names_to='date',cols=5:ncol(data_con)) %>%
  group_by(Province.State,Country.Region,date,Lat,Long) %>%
  summarise("confirmed"=sum(value,na.rm=TRUE))
##View(data_con_sub)
data_dec_sub<-data_dec %>%
  pivot_longer(names_to="date",cols=5:ncol(data_dec)) %>%
  group_by('Province/State','Country/Region',date,Lat,Long) %>%
  summarise("deceased"=sum(value,na.rm=TRUE))
data_evolution<-data_con_sub %>%
  full_join(data_dec_sub) %>%
  ungroup() %>%
  
  arrange(as.Date(date,format="%m/%d/%Y")) %>%
  group_by('Province/State','Country/Region',Lat,Long) %>%
  mutate(
    recovered=lag(confirmed,14,default = 0) - deceased,
    recovered=ifelse(recovered>0,recovered,0),
    active= confirmed - recovered - deceased
    
  ) %>%
  pivot_longer(names_to="var",cols=c(confirmed ,recovered ,deceased ,active)) %>%
  ungroup()
data_evolution %>% filter(Country.Region=='China') %>% head(10) %>%
  kable('latex', booktabs=T, caption='Raw Data (with first 10 Columns Only)',
        format.args=list(big.mark=',')) %>%
  kable_styling(latex_options = c('striped', 'hold_position', 'repeat_header'))




x <- raw.data.confirmed
x$confirmed <- x[, ncol(x)]
x %<>% select(c(Country.Region, Province.State, Lat, Long, confirmed)) %>%
  mutate(txt=paste0(Country.Region, ' - ', Province.State, ': ', confirmed))
m <- leaflet(width=1200, height=800) %>% addTiles()
# circle marker (units in pixels)
m %<>% addCircleMarkers(x$Long, x$Lat,
                        # radius=2+log2(x$confirmed),
                        radius=0.03*sqrt(x$confirmed),
                        stroke=F,
                        color='Blue', fillOpacity=0.3,
                        popup=x$txt)
# world
m





axis.text=element_text(size=7)
axis.text.x=element_text(angle=45, hjust=1) +
  facet_wrap(~type, ncol=1, scales='free_y')
axis.text




x <- raw.data.confirmed
x$confirmed <- x[, ncol(x)]
x %<>% select(c(Country.Region, Province.State, Lat, Long, confirmed)) %>%
  mutate(txt=paste0(Country.Region, ' - ', Province.State, ': ', confirmed))
m <- leaflet(width=1200, height=800) %>% addTiles()
# circle marker (units in pixels)
m %<>% addCircleMarkers(x$Long, x$Lat,
                        # radius=2+log2(x$confirmed),
                        radius=0.03*sqrt(x$confirmed),
                        stroke=F,
                        color='Blue', fillOpacity=0.3,
                        popup=x$txt)
# world
m





axis.text=element_text(size=7)
axis.text.x=element_text(angle=45, hjust=1) +
  facet_wrap(~type, ncol=1, scales='free_y')
axis.text