library(shiny)
library(ggplot2)
library(wordcloud)
library(plotly)
library(shinythemes)
library(RColorBrewer)
library(DT)
# read data for question 1
q1<-read.csv('q1.csv')

# read data for question 2
q2<-read.csv('q2.csv')
q2.point.rank<-read.csv('q2_rank.csv') # data for top20 ranking
q2.point.amount<-read.csv('q2_amount.csv') # data for top20 amount
q2.image<-read.csv('q2_img.csv')# top 10 board game for top20 type seperately
# data for wordcloud
q2.wordcloud<-q2
q2.wordcloud$average_rank<-(4598/q2.wordcloud$average_rank)^4
# data for circular chart
# process data by ranking order
q2.rank<-q2[order(q2$average_rank),]
q2.rank$id<-seq(1,83)
q2.rank <- head(q2.rank, 20)
number_of_bar <- nrow(q2.rank)
angle <-  90 - 360 * (q2.rank$id-0.5) /number_of_bar
q2.rank$hjust<-ifelse( angle < -90, 1, 0)
q2.rank$angle<-ifelse(angle < -90, angle+180, angle)
# process data by amount
q2.amount<-q2[order(q2$amount),]
q2.amount$id<-seq(83,1,-1)
q2.amount <- tail(q2.amount, 20)
number_of_bar <- nrow(q2.amount)
angle <-  90 - 360 * (q2.amount$id-0.5) /number_of_bar
q2.amount$hjust<-ifelse( angle < -90, 1, 0)
q2.amount$angle<-ifelse(angle < -90, angle+180, angle)

# read data for question 3
q3<-read.csv('q3.csv')
q3.point.rank<-read.csv('q3_rank.csv')
q3.point.amount<-read.csv('q3_amount.csv')
q3.image<-read.csv('q3_img.csv')# top 10 board game for top20 mechanic seperately
# data for wordcloud
q3.wordcloud<-q3
q3.wordcloud$average_rank<-(1561/q3.wordcloud$average_rank)^1.5
# data for circular chart
# process data by ranking order
q3.rank<-q3[order(q3$average_rank),]
q3.rank$id<-seq(1,53)
q3.rank <- head(q3.rank, 20)
number_of_bar <- nrow(q3.rank)
angle <-  90 - 360 * (q3.rank$id-0.5) /number_of_bar
q3.rank$hjust<-ifelse( angle < -90, 1, 0)
q3.rank$angle<-ifelse(angle < -90, angle+180, angle)
# process data by amount
q3.amount<-q3[order(q3$amount),]
q3.amount$id<-seq(53,1,-1)
q3.amount <- tail(q3.amount, 20)
number_of_bar <- nrow(q3.amount)
angle <-  90 - 360 * (q3.amount$id-0.5) /number_of_bar
q3.amount$hjust<-ifelse( angle < -90, 1, 0)
q3.amount$angle<-ifelse(angle < -90, angle+180, angle)

# ui R
ui<-navbarPage(theme = shinythemes::shinytheme("cosmo"),
               title='Board Game Analysis',
               # panel for question 1
               tabPanel('year',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('yearRange','please select a year range',
                                        choices=c('1981-1990','1991-2000','2001-2010','2011-2020')),
                            plotlyOutput('bar'),
                            h5('This bar chart shows the amount of board game of each ten years')),
                          mainPanel(plotlyOutput('line'),
                                    h5('This line chart shows the trend of average ranking of board game from 1981 to 2020,
                                       except year 2020, the trend is inreases,especially from 2005 to 2019.
                                       The reason that the average ranking in 2020 is lower than 2019 is maybe 
                                       it is only June on 2020.'))
                           )
                        ),
               # panel for question 2
               tabPanel('type',
                        sidebarLayout(
                          sidebarPanel(
                            textInput('typename','please enter a board game type from the top 20 if you want:'),
                            sliderInput('yearrange','range of year:', min = 1981, max = 2020, value=c(1981,2020)),
                            plotlyOutput('siderplottype')),
                          mainPanel(tabsetPanel(
                            tabPanel('word cloud for total type',radioButtons('wordcloudtype','Please select an aspect first:', 
                                                 choices = c('ranking','amount')),
                                    plotOutput('wordcloudtype')),
                            tabPanel('circular chart for top 20 type',radioButtons('circulartype','Please select an aspect first:', 
                                                                                   choices = c('ranking','amount')),        
                                    plotOutput('circulartype')),
                            tabPanel('top10 board game for each top20 type',
                                     DT::dataTableOutput('tabletype')))
                        ))),
               # panel for question 3
               tabPanel('mechanic',
                        sidebarLayout(
                         sidebarPanel(
                           textInput('mechanicname','please enter a board game mechanic from the top 20 if you want:'),
                           sliderInput('yearmechanic','range of year:', min = 1981, max = 2020, value=c(1981,2020)),
                           plotlyOutput('siderplotmechanic')),
                         mainPanel(tabsetPanel(
                           tabPanel('word cloud for total mechanic',radioButtons('wordcloudmechanic','Please select an aspect first:', 
                                                                                     choices = c('ranking','amount')),
                                    plotOutput('wordcloudmechanic')),
                           tabPanel('circular chart for top 20 mechanic',radioButtons('circularmechanic','Please select an aspect first:', 
                                                                                  choices = c('ranking','amount')),        
                                    plotOutput('circularmechanic')),
                           tabPanel('top10 board game for each top20 mechanic',
                                    DT::dataTableOutput('tablemechanic')))
               ))))

# server R
server<-function(input,output){
  # line chart for question 1(year panel)
  output$line<-renderPlotly({
    lineplot<-ggplot(q1,aes(year,average_rank))+
      geom_line(color='#E7A23F',size=1.5)+
      geom_point(color='#FA5D19',size=2)+
      labs(y='average ranking',x='year',title='chaning of average ranking from 1981 to 2020')+
      scale_y_continuous(trans='reverse')
    ggplotly(lineplot)
  })
  # bar chart for question 1(year panel)
  output$bar<-renderPlotly({
    # for year 1981 to 1990
    if (input$yearRange=='1981-1990'){
      range1<-q1[q1$year<=1990,]
      ggplotly(ggplot(range1,aes(year,amount))+
        geom_bar(stat='identity',fill='#F9723D')+
        labs(y='amount',x='year'))
    }
    # for year 1991 to 2000
    else if (input$yearRange=='1991-2000'){
      range2<-q1[1991<=q1$year & q1$year<=2000,]
      ggplotly(ggplot(range2,aes(year,amount))+
        geom_bar(stat='identity',fill='#F9723D')+
        labs(y='amount',x='year'))
    }
    # for year 2001 to 2010
    else if (input$yearRange=='2001-2010'){
      range3<-q1[2001<=q1$year & q1$year<=2010,]
      ggplotly(ggplot(range3,aes(year,amount))+
        geom_bar(stat='identity',fill='#F9723D')+
        labs(y='amount',x='year'))
    }
    # for year 2011 to 2020
    else if (input$yearRange=='2011-2020'){
      range4<-q1[2011<=q1$year & q1$year<=2020,]
      ggplotly(ggplot(range4,aes(year,amount))+
        geom_bar(stat='identity',fill='#F9723D')+
        labs(y='amount',x='year'))
    }
    
  })
  # word cloud for question 2(type panel)
  output$wordcloudtype<-renderPlot({
    # for each type ranking
    if (input$wordcloudtype=='ranking'){
      wordcloud(words = q2.wordcloud$type, freq = q2.wordcloud$average_rank, min.freq = 0,
                max.words=50, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    }
    # for each type amount
    else if (input$wordcloudtype=='amount'){
      wordcloud(words = q2$type, freq = q2$amount, min.freq = 1,
                max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    }
  }, height = 500, width = 600)
  # circular chart for question 2(type panel)
  output$circulartype<-renderPlot({
    if (input$circulartype=='ranking'){
      ggplot(q2.rank, aes(x=as.factor(id), y=average_rank)) + 
        geom_bar(stat="identity", fill=alpha("#FBB957", 0.7)) +
        ylim(-4000,14000) +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "cm")      
        ) +
        coord_polar(start = 0) +
        geom_text(data=q2.rank, aes(x=id, y=average_rank-1700, label=type, hjust=hjust), 
                  color="black", fontface="bold",alpha=0.6, size=4.5, angle= q2.rank$angle, inherit.aes = FALSE )
    }
    else if (input$circulartype=='amount'){
      ggplot(q2.amount, aes(x=as.factor(id), y=amount)) + 
        geom_bar(stat="identity", fill=alpha("#FBB957", 0.7)) +
        ylim(-400,5000) +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "cm")      
        ) +
        coord_polar(start = 0) +
        geom_text(data=q2.amount, aes(x=id, y=amount-50, label=type, hjust=hjust), 
                  color="black", fontface="bold",alpha=0.6, size=4, angle= q2.amount$angle, inherit.aes = FALSE )
    }
  }, height = 600, width = 600)
  # line or bar chart for question 2(type panel)
  output$siderplottype<-renderPlotly({
    typeranking<-tolower(as.character(unique(q2.point.rank$type)))
    typeamount<-tolower(as.character(unique(q2.point.amount$type)))
    yeartype<-seq(input$yearrange[1],input$yearrange[2],1)
    q2.point.rank.new<-q2.point.rank[q2.point.rank$year %in% yeartype,]
    q2.point.amount.new<-q2.point.amount[q2.point.amount$year %in% yeartype,]
    if ((tolower(input$typename) %in% typeranking) & (input$wordcloudtype=='ranking')){
      ggplotly(ggplot(q2.point.rank.new[tolower(q2.point.rank.new$type)==tolower(input$typename),],aes(x=year,y=average_rank))+
        geom_line(color='#F86B1D',size=1)+
        scale_y_reverse()+
        labs(y='average rank'))
    }
    else if ((tolower(input$typename) %in% typeamount) & (input$wordcloudtype=='amount')){
      ggplotly(ggplot(q2.point.amount.new[tolower(q2.point.amount.new$type)==tolower(input$typename),],aes(x=year,y=amount))+
        geom_bar(stat = 'identity',fill='#F86B1D'))
    }
    else if ((tolower(input$typename) %in% typeranking) & (input$circulartype=='ranking')){
      ggplotly(ggplot(q2.point.rank.new[tolower(q2.point.rank.new$type)==tolower(input$typename),],aes(x=year,y=average_rank))+
        geom_line(color='#F86B1D',size=1)+
        scale_y_reverse()+
        labs(y='average rank'))
    }
    else if ((tolower(input$typename) %in% typeamount) & (input$circulartype=='amount')){
      ggplotly(ggplot(q2.point.amount.new[tolower(q2.point.amount.new$type)==tolower(input$typename),],aes(x=year,y=amount))+
        geom_bar(stat = 'identity',fill='#F86B1D'))
    }
  })
  # table for question 2(type panel)
  output$tabletype<-DT::renderDataTable({
      q2.image$image <- paste('<img src="',q2.image$image,'" height="52">')
      data <- q2.image[,-1]
      DT::datatable(data,escape = FALSE, options = list(dom = 'tp')
                    , filter = list(position = "top"))%>%
        DT::formatStyle(names(data))
  })
  # word cloud for question 3(mechanic panel)
  output$wordcloudmechanic<-renderPlot({
    # for each mechanic ranking
    if (input$wordcloudmechanic=='ranking'){
      wordcloud(words = q3.wordcloud$mechanic, freq = q3.wordcloud$average_rank, min.freq = 0,
                max.words=50, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    }
    # for each mechanic amount
    else if (input$wordcloudmechanic=='amount'){
      wordcloud(words = q3$mechanic, freq = q3$amount, min.freq = 1,
                max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    }
  }, height = 500, width = 600)
  # circular chart for question 3(mechanic panel)
  output$circularmechanic<-renderPlot({
    if (input$circularmechanic=='ranking'){
      ggplot(q3.rank, aes(x=as.factor(id), y=average_rank)) + 
        geom_bar(stat="identity", fill=alpha("#FBB957", 0.7)) +
        ylim(-4000,14000) +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "cm")      
        ) +
        coord_polar(start = 0) +
        geom_text(data=q3.rank, aes(x=id, y=average_rank-1500, label=mechanic, hjust=hjust), 
                  color="black", fontface="bold",alpha=0.6, size=4.5, angle= q3.rank$angle, inherit.aes = FALSE )
    }
    else if (input$circularmechanic=='amount'){
      ggplot(q3.amount, aes(x=as.factor(id), y=amount)) + 
        geom_bar(stat="identity", fill=alpha("#FBB957", 0.7)) +
        ylim(-400,5000) +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "cm")      
        ) +
        coord_polar(start = 0) +
        geom_text(data=q3.amount, aes(x=id, y=amount-100, label=mechanic, hjust=hjust), 
                  color="black", fontface="bold",alpha=0.6, size=4.5, angle= q3.amount$angle, inherit.aes = FALSE )
    }
  }, height = 600, width = 600)
  # line or bar chart for question 3(mechanic panel)
  output$siderplotmechanic<-renderPlotly({
    mechanicranking<-tolower(as.character(unique(q3.point.rank$mechanic)))
    mechanicamount<-tolower(as.character(unique(q3.point.amount$mechanic)))
    yearmechanic<-seq(input$yearmechanic[1],input$yearmechanic[2],1)
    q3.point.rank.new<-q3.point.rank[q3.point.rank$year %in% yearmechanic,]
    q3.point.amount.new<-q3.point.amount[q3.point.amount$year %in% yearmechanic,]
    if ((tolower(input$mechanicname) %in% mechanicranking) & (input$wordcloudmechanic=='ranking')){
      ggplotly(ggplot(q3.point.rank.new[tolower(q3.point.rank.new$mechanic)==tolower(input$mechanicname),],aes(x=year,y=average_rank))+
        geom_line(color='#F86B1D')+
        scale_y_reverse()+
        labs(y='average rank'))
    }
    else if ((tolower(input$mechanicname) %in% mechanicamount) & (input$wordcloudmechanic=='amount')){
      ggplotly(ggplot(q3.point.amount.new[tolower(q3.point.amount.new$mechanic)==tolower(input$mechanicname),],aes(x=year,y=amount))+
        geom_bar(stat = 'identity',fill='#F86B1D'))
    }
    else if ((tolower(input$mechanicname) %in% mechanicranking) & (input$circularmechanic=='ranking')){
      ggplotly(ggplot(q3.point.rank.new[tolower(q3.point.rank.new$mechanic)==tolower(input$mechanicname),],aes(x=year,y=average_rank))+
        geom_line(color='#F86B1D')+
        scale_y_reverse()+
        labs(y='average rank'))
    }
    else if ((tolower(input$mechanicname) %in% mechanicamount) & (input$circulartype=='amount')){
      ggplotly(ggplot(q3.point.amount.new[tolower(q3.point.amount.new$mechanic)==tolower(input$mechanicname),],aes(x=year,y=amount))+
        geom_bar(stat = 'identity',fill='#F86B1D'))
    }
  })
  # table for question 3(mechanic panel)
  output$tablemechanic<-DT::renderDataTable({
    q3.image$image <- paste('<img src="',q3.image$image,'" height="52">')
    data <- q3.image[,-1]
    DT::datatable(data,escape = FALSE, options = list(dom = 'tp')
                  , filter = list(position = "top"))%>%
      DT::formatStyle(names(data))
  })
}
shinyApp(ui=ui,server=server)

