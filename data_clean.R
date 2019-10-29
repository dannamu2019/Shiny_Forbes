install.packages('readr')
library(readr)

# 2019 data clean
data2019 = read.csv('Forbes2019.csv',stringsAsFactors = FALSE)
str(data2019)

data2019$Sales...millions. = parse_number(data2019$Sales...millions.)
data2019$Profits...millions. = parse_number(data2019$Profits...millions.)
data2019$Assets...millions. = parse_number(data2019$Assets...millions.)
data2019$Market.Value..As.of.4.18.19...m.= parse_number(data2019$Market.Value..As.of.4.18.19...m.)

write.csv(data2019, file='data2019.csv')

# 2018 data clean
data2018 = read.csv('Forbes2018.csv',stringsAsFactors = FALSE)
str(data2018)

data2018$Sales...millions. = parse_number(data2018$Sales...millions.)
data2018$Profits...millions. = parse_number(data2018$Profits...millions.)
data2018$Assets...millions. = parse_number(data2018$Assets...millions.)
data2018$Market.Value..As.of.3.29.18...m. = parse_number(data2018$Market.Value..As.of.3.29.18...m.)

write.csv(data2018, file='data2018.csv')

#  2017 data clean
data2017 = read.csv('Forbes2017.csv',stringsAsFactors = FALSE)
str(data2017)

data2017$Profits = parse_number(data2017$Profits)
data2017$Assets = parse_number(data2017$Assets)
data2017$Market.Value = parse_number(data2017$Market.Value)

write.csv(data2017, file='data2017.csv')

# 2018_USA data clean

data2018_USA = read.csv('Fortune2018_USA.csv',stringsAsFactors = FALSE)
str(data2018_USA)

data2018_USA$Previous.Rank = parse_number(data2018_USA$Previous.Rank)
sub_data2018_USA = data2018_USA[!is.na(data2018_USA$Previous.Rank),]
dim(sub_data2018_USA)

data2018_USA$Revenues...M. = parse_number(data2018_USA$Revenues...M.)
data2018_USA$Revenue.Change = parse_number(data2018_USA$Revenue.Change)
data2018_USA$Profits...M. = parse_number(data2018_USA$Profits...M.)
data2018_USA$Profit.Change = parse_number(data2018_USA$Profit.Change)

data2018_USA$Assets...M.= parse_number(data2018_USA$Assets...M.)
data2018_USA$Mkt.Value.as.of.3.29.18...M. = parse_number(data2018_USA$Mkt.Value.as.of.3.29.18...M.)
data2018_USA$Employees = parse_number(data2018_USA$Employees)
data2018_USA$Years.on.Fortune.500.List = parse_number(data2018_USA$Years.on.Fortune.500.List)
str(data2018)

write.csv(data2018_USA, file='data2018_USA.csv')

# sum revernue, profit, asset, market value

data2019=read.csv('data2019.csv')
a1=sum(as.numeric(data2019$Sales...millions.), na.rm = TRUE)
a2=sum(as.numeric(data2019$Profits...millions.),na.rm =TRUE)
a3=sum(as.numeric(data2019$Assets...millions.),na.rm =TRUE)
a4=sum(as.numeric(data2019$Market.Value..As.of.4.18.19...m.),na.rm =TRUE)

data2018=read.csv('data2018.csv')
b1=sum(as.numeric(data2018$Revenues...millions.), na.rm = TRUE)
b2=sum(as.numeric(data2018$Profits...millions.),na.rm =TRUE)
b3=sum(as.numeric(data2018$Assets...millions.),na.rm =TRUE)
b4=sum(as.numeric(data2018$Market.Value..As.of.3.29.18...m.),na.rm =TRUE)

data2017=read.csv('data2017.csv')
c1=sum(as.numeric(data2017$Sales), na.rm = TRUE)
c2=sum(as.numeric(data2017$Profits), na.rm = TRUE)
c3=sum(as.numeric(data2017$Assets), na.rm = TRUE)
c4=sum(as.numeric(data2017$Market.Value), na.rm = TRUE)

# create table

total <- matrix(c(a1,a2,a3,a4,b1,b2,b3,b4),ncol=4,byrow=TRUE)
colnames(total) <- c('Revenue','profits','assets','mkt.value')
rownames(total) <- c('2019','2018')
x = colnames(total)
y = rownames(total)
total<- as.table(total)

total2019 <- matrix(c(a1,a2,a3,a4),ncol=4,byrow=TRUE)
colnames(total) <- c('Revenue','profits','assets','mkt.value')
rownames(total) <- c('2019')
x = colnames(total)
y = rownames(total)

# filter column by country name, sum and descending.

data2019=read.csv('data2019.csv')
head(data2019)
table2019=data2019%>%
  group_by(.,Country)%>%
  summarise(.,count=n())%>%
  arrange(desc(count))%>%
  top_n(10)

data2018=read.csv('data2018.csv')
table2018=data2018%>%
  group_by(.,Country)%>%
  summarise(.,count=n())%>%
  arrange(desc(count))%>%
  top_n(10)

data2017=read.csv('data2017.csv')
table2017=data2017%>%
  group_by(.,Country)%>%
  summarise(.,count=n())%>%
  arrange(desc(count))%>%
  top_n(10)

data2018_USA=read.csv('data2018_USA.csv')
table2018_USA=data2018_USA%>%
  group_by(.,State)%>%
  summarise(.,count=n())%>%
  arrange(desc(count))%>%
  top_n(10)

# filter column by industries name, sum num and profit.

# 2017 num
data2017=read.csv('data2017.csv')
head(data2017)
table2017=data2017%>%
  group_by(.,Sector)%>%
  summarise(.,count=n())%>%
  arrange(desc(count))%>%
  top_n(11)
table2017 = table[2:11,]
  
# 2018_USA num
data2018_USA=read.csv('data2018_USA.csv')
head(data2018_USA)
table2018_num=data2018_USA%>%
  group_by(.,Sector)%>%
  summarise(.,count=n())%>%
  arrange(desc(count))%>%
  top_n(10)

# profits
table2018_profits=data2018_USA%>%
  group_by(.,Sector)%>%
  summarise(.,Profits=sum(Profits...M.))%>%
  arrange(desc(Profits))%>%
  top_n(10)

#2018_USA -profit analysis
data2018_PC_rank=data2018_USA%>%arrange(desc(data2018_USA$Profit.Change))%>%top_n(100)
data2018_PC=data2018_USA%>%arrange(desc(data2018_USA$Profit.Change))%>%top_n(10)




  



