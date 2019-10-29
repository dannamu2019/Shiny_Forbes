# Define server logic required to draw a histogram
shinyServer(function(input, output){
  # 1.2019,2018 total ave/pro/asset/mv(Intro)
  output$total_stats <- renderPlotly({
    total <- matrix(c(a1,a2,a3,a4,b1,b2,b3,b4),ncol=4,byrow=TRUE)
    colnames(total) <- c('Revenue','profits','assets','mkt.value')
    rownames(total) <- c('2019','2018')
    x = colnames(total)
    y = rownames(total)
    total<- as.table(total)
    total=as.data.frame(total)
    colnames(total) = c('year','type','value')
    
    ggplotly(ggplot(total,aes(x=reorder(type,value),y=value,fill=year))+
      geom_bar(stat='identity',position = 'dodge'))
    
  })
  # Box
  output$TotalAveBox <- renderInfoBox({
    total_ave_value = sum(as.numeric(data2019$Sales...millions.), na.rm = TRUE)
    infoBox('TOTAL AVE',
            total_ave_value,
            icon = icon("hand-o-up"),
            color = "green")
  })
  
  output$TotalProfitsBox <- renderInfoBox({
    total_profits_value = sum(as.numeric(data2019$Profits...millions.),na.rm =TRUE)
    infoBox('TOTAL PROFITS',
            total_profits_value,
            icon = icon("hand-o-up"),
            color = "green")
  })
  output$TotalAssetBox <- renderInfoBox({
    total_assets_value = sum(as.numeric(data2019$Assets...millions.),na.rm =TRUE)
    infoBox('TOTAL ASSETS',
            total_assets_value,
            icon = icon("hand-o-up"),
            color = "green")
  })
  output$TotalMktVBox <- renderInfoBox({
    total_Mkt_value = sum(as.numeric(data2019$Market.Value..As.of.4.18.19...m.),na.rm =TRUE)
    infoBox('TOTAL MKT VALUE',
            total_Mkt_value,
            icon = icon("hand-o-up"),
            color = "green")
  })
  
  # 2.019,2018,2017 Top Profits Company(companies)
  output$top_profits <- renderPlotly({
    
    if (input$year == '2017'){
      data2017_top10<-data2017[1:10,]
      ggplotly(data2017_top10 %>% ggplot(aes(x=reorder(Company,Profits),y=Profits)) + geom_bar(stat = 'identity',aes(fill = Profits)) + coord_flip()+
        labs(title='2017 Most Profit Company'))
    }
    else if (input$year == '2018'){
      data2018_top10 <-data2018[1:10,]
      ggplotly(data2018_top10 %>%
        ggplot(aes(x=reorder(Company.Name,Profits...millions.),y=Profits...millions.)) +
        geom_bar(stat = 'identity',aes(fill = Profits...millions.))+
        labs(title='2018 Most Profits Company') +
        coord_flip())
    }
    else if (input$year == '2018_USA'){
      data2018_USA_top10<-data2018_USA[1:10,]
      ggplotly(data2018_USA_top10 %>% ggplot(aes(x=reorder(title,Profits...M.),y=Profits...M.)) + geom_bar(stat = 'identity',aes(fill = Profits...M.)) + coord_flip()+
        labs(title='2018_USA Most Profit Company'))
    }
    else if (input$year == '2019'){
      data2019_top10 <-data2019[1:10,]
      ggplotly(data2019_top10 %>% ggplot(aes(x=reorder(Company.Name,Profits...millions.),y=Profits...millions.)) +
        geom_bar(stat = 'identity',aes(fill = Profits...millions.))+
        labs(title='2019 Most Profits Company')+
        coord_flip())
    }
    
  })
  
  output$breakdown_countries <- renderPlot({
    if (input$year == '2017'){
      table2017=data2017%>%
        group_by(.,Country)%>%
        summarise(.,count=n())%>%
        arrange(desc(count))%>%
        top_n(10)
      country2017 = merge(data2017,table2017,by='Country')
      ggplot(country2017,aes(x=1))+
        geom_bar(aes(fill=Country))+
        coord_polar(theta="y")
    }
    else if (input$year == '2018'){
      table2018=data2018%>%
        group_by(.,Country)%>%
        summarise(.,count=n())%>%
        arrange(desc(count))%>%
        top_n(10)
      country2018 = merge(data2018,table2018,by='Country')
      ggplot(country2018,aes(x=1))+
        geom_bar(aes(fill=Country))+
        coord_polar(theta="y")
    }
    else if (input$year == '2018_USA'){
      table2018_USA=data2018_USA%>%
        group_by(.,State)%>%
        summarise(.,count=n())%>%
        arrange(desc(count))%>%
        top_n(10)
      State2018_USA = merge(data2018_USA,table2018_USA,by='State')
      ggplot(State2018_USA,aes(x=1))+
        geom_bar(aes(fill=State))+
        coord_polar(theta="y")
    }
    else if (input$year == '2019'){
      table2019=data2019%>%
        group_by(.,Country)%>%
        summarise(.,count=n())%>%
        arrange(desc(count))%>%
        top_n(10)
      country2019 = merge(data2019,table2019,by='Country')
      ggplot(country2019,aes(x=1))+
        geom_bar(aes(fill=Country))+
        coord_polar(theta="y")
    }
  })

  # 4 Top industries of the Global 2000?
  output$breakdown_sector2017<-renderPlotly({
    table2017=data2017%>%
      group_by(.,Sector)%>%
      summarise(.,count=n())%>%
      arrange(desc(count))%>%
      top_n(11)
    table2017 = table2017[2:11,]
    ggplotly(table2017 %>%
      ggplot(aes(x=reorder(Sector, count),y=count))+
      geom_bar(stat='identity',aes(fill=Sector))+ labs(title='2017 Sector') +
      coord_flip())
  })
  
  output$breakdown_sector2018USA<-renderPlotly({
    table2018_num=data2018_USA%>%
      group_by(.,Sector)%>%
      summarise(.,count=n())%>%
      arrange(desc(count))%>%
      top_n(10)
    ggplotly(table2018_num %>% 
      ggplot(aes(x=reorder(Sector, count),y=count))+
      geom_bar(stat='identity',aes(fill=Sector))+ labs(title='2018 USA Sector') +
      coord_flip())
  })
  # profit
  output$breakdown_profit2018USA<-renderPlotly({
    table2018_profits=data2018_USA%>%
      group_by(.,Sector)%>%
      summarise(.,Profits=sum(Profits...M.))%>%
      arrange(desc(Profits))%>%
      top_n(10)
    ggplotly(table2018_profits %>%
               ggplot(aes(x=reorder(Sector, Profits),y=Profits))+
               geom_bar(stat='identity',aes(fill=Sector))+ labs(title='2018 USA Profit') +
               coord_flip())
  })
  # 6 2018_USA -profit analysis
  # rank Volitility
  output$breakdown_rankvol2018USA<-renderPlotly({
    data2018_PC_rank=data2018_USA%>%arrange(desc(data2018_USA$Profit.Change))%>%top_n(100)
    ggplotly(data2018_PC_rank %>% ggplot(aes(x=rank, y=Profit.Change))+
      geom_point(aes(color="red"),position="jitter") + labs(title="2018 USA Rank Volatility"))
  })
  
  # Sector Volitility
  output$breakdown_sectorvol2018USA<-renderPlotly({
    data2018_PC=data2018_USA%>%arrange(desc(data2018_USA$Profit.Change))%>%top_n(10)
    ggplotly(data2018_PC %>% ggplot(aes(x=Sector, y=Profit.Change))+
               geom_point(aes(color=Sector),position="jitter") + labs(title="2018 USA Sector Volatility"))
  })
  
  output$dynamic_data_table <- DT::renderDataTable({
    if (input$year == '2017'){
      data2017
    }
    else if (input$year == '2018'){
      data2018
    }
    else if (input$year == '2018_USA'){
      data2018_USA
    }
    else if (input$year == '2019'){
      data2019
    }
  })

})






