library(ggplot2)
library(tidyverse)
library(dplyr)
library(treemap)
library(maps)
library(leaflet)
library(ggthemes)
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()



data2019=read.csv('data2019.csv')
data2018=read.csv('data2018.csv')
data2017=read.csv('data2017.csv')
data2018_USA=read.csv('data2018_USA.csv')

# 1. Compare 2019,2018 total ave/pro/asset/mv

total=as.data.frame(total)
colnames(total) = c('year','type','value')
ggplot(total,aes(x=reorder(type,value),y=value,fill=year))+
  geom_bar(stat='identity',position = 'dodge')

# 2. 2019,2018,2017 Top Profits Company

data2019_top10 <-data2019[1:10,]
data2019_top10 %>% ggplot(aes(x=reorder(Company.Name,Profits...millions.),y=Profits...millions.)) + 
  geom_bar(stat = 'identity',aes(fill = Profits...millions.))+
  labs(title='2019 Most Profits Company')+
  coord_flip()+
  theme_solarized()

data2018_top10 <-data2018[1:10,]
data2018_top10 %>% 
  ggplot(aes(x=reorder(Company.Name,Profits...millions.),y=Profits...millions.)) + 
  geom_bar(stat = 'identity',aes(fill = Profits...millions.))+ 
  labs(title='2018 Most Profits Company') +
  coord_flip()+
  theme_pander()

data2017_top10<-data2017[1:10,]
data2017_top10 %>% ggplot(aes(x=reorder(Company,Profits),y=Profits)) + geom_bar(stat = 'identity',aes(fill = Profits)) + coord_flip()+
 labs(title='2017 Most Profit Company')

data2018_USA_top10<-data2018_USA[1:10,]
data2018_USA_top10 %>% ggplot(aes(x=reorder(title,Profits...M.),y=Profits...M.)) + geom_bar(stat = 'identity',aes(fill = Profits...M.)) + coord_flip()+
  labs(title='2018_USA Most Profit Company')+theme_solarized()

# 3 2019,2018,2017 Where The Worlds Largest Companies Are? (pie)
# add num in the pie?

country2019 = merge(data2019,table2019,by='Country')
ggplot(country2019,aes(x=1))+
  geom_bar(aes(fill=Country))+
  coord_polar(theta="y")

country2018 = merge(data2018,table2018,by='Country')
ggplot(country2018,aes(x=1))+
  geom_bar(aes(fill=Country))+
  coord_polar(theta="y")

country2017 = merge(data2017,table2017,by='Country')
ggplot(data2017,aes(x=1))+
  geom_bar(aes(fill=Country))+
  coord_polar(theta="y")

State2018_USA = merge(data2018_USA,table2018_USA,by='State')
ggplot(State2018_USA,aes(x=1))+
  geom_bar(aes(fill=State))+
  coord_polar(theta="y")

# USA vs China (pic)

# 4 Top industries of the Global 2000? (bar)
# 2017 num
table2017 %>%
  ggplot(aes(x=reorder(Sector, count),y=count))+
  geom_bar(stat='identity',aes(fill=Sector))+
  coord_flip()

# 5 top industries of the USA?
# 2018_USA num
table2018_num %>%
  ggplot(aes(x=reorder(Sector, count),y=count))+
  geom_bar(stat='identity',aes(fill=Sector))+
  coord_flip()

# 2018_USA which sector saw the largest profit gain?

table2018_profits %>%
  ggplot(aes(x=reorder(Sector, Profits),y=Profits))+
  geom_bar(stat='identity',aes(fill=Sector))+
  coord_flip()

# 6 2018_usa rank volitility
# rank Volitility
data2018_PC_rank %>% ggplot(aes(x=rank, y=Profit.Change))+
  geom_point(aes(color="red"),position="jitter")
# Sector Volitility
data2018_PC %>% ggplot(aes(x=Sector, y=Profit.Change))+
  geom_point(aes(color=Sector),position="jitter")





