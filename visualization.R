library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(DT)
library(tidyr)

lakes <- read_csv("lake_areas.csv")
print(lakes[1])

ui <- fluidPage(
  sidebarPanel(
    sliderInput("area", "Area:", min = 0, max = max(lakes$area), value = c(0,max(lakes$area)), step=0.001),
    uiOutput("lakeSelector"),
    uiOutput("basinSelector"),
    width=12
  ),
  
  plotOutput("plot", brush = "plot_brush", height = 500, dblclick = "plot_reset"),
  DT::dataTableOutput('table')
)
server <- function(input, output, session) {

  output$plot <- renderPlot({
    y <- selected() %>%
      top_n(1e3)
    print(y)
    
    if (length(input$sub_basin)!=0){
      ggplot() +
        geom_point(data=filter(lakes, Sub_Basin %in% input$sub_basin),aes(time, area),size = 1) +
        geom_point(data=filter(y, Sub_Basin %in% input$sub_basin), aes(time, area), size = 1, color="orange") +
        geom_line(data=filter(lakes, Sub_Basin %in% input$sub_basin),aes(time, area, group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")
    }
    else if(length(input$lakeID)!=0){
      ggplot() +
        geom_point(data=filter(lakes, GL_ID %in% input$lakeID),aes(time, area),size = 1) +
        geom_point(data=filter(y, GL_ID %in% input$lakeID), aes(time, area), size = 1, color="orange") +
        geom_line(data=filter(lakes, GL_ID %in% input$lakeID),aes(time, area, group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")
    }
    else{
      ggplot()+
        geom_point(data=lakes,aes(time, area),size = 1) +
        geom_point(data=y, aes(time, area),  size = 1, color="orange") +
        geom_line(data=lakes,aes(time, area, group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10() +
        ylim(input$area[1],input$area[2]) +
        xlab("Year")
    } 
    
  },
  res = 96, height = 250)
  
  selected <- reactive({
    #df<-filter(lakes,(Sub_Basin %in% input$sub_basin)&(GL_ID %in% input$lakeID))
    df<-if(length(input$sub_basin)!=0){filter(lakes, Sub_Basin %in% input$sub_basin) }else{lakes}
    df<-if(length(input$lakeID)!=0){filter(df, GL_ID %in% input$lakeID)}
    if(length(input$sub_basin)!=0|length(input$lakeID)!=0){
      brushedPoints(df, input$plot_brush, "time", "area") %>%
        select(GL_ID, time, area, Sub_Basin)
    }
    else{
      brushedPoints(lakes, input$plot_brush, "time", "area") %>%
        select(GL_ID, time, area, Sub_Basin)
    }
  })
  
  output$table = DT::renderDataTable({
    x <- selected() %>%
      select(GL_ID,time) %>% 
      top_n(1e3)
    #print(x)
    
    if (nrow(x) == 0) {
      return()
    }
    
    base <- "https://glaciersblob.blob.core.windows.net/lakes/thumbnails/sentinel/"
    df<-data.frame(id=NA,year=NA,link=NA)
    
    for (i in seq_len(nrow(x))) {
      time<-as.character(x$time[i])
      link<-str_c('<img src="',base, x$GL_ID[i], "_", x$time[i], '.png" height="52" title="', time, '"></img>')
      year<-as.numeric(substr(time,1,4))
      new<-c(x$GL_ID[i],year,link)
      df<-rbind(df,new)
    }
    
    df<-df[-1,] %>%
      arrange(year)%>%
      pivot_wider(names_from=year,values_from=link)
    
    
    DT::datatable(df, escape = FALSE)
  })
  
  
  output$lakeSelector <- renderUI({
    if(length(input$sub_basin)==0) {
      selectizeInput(inputId = "lakeID",
                     label = NULL,
                     choices = c("Please choose a lake ID" = "",lakes$GL_ID),
                     multiple = TRUE)
    }
    else{
      cur_lakes <- lakes %>%
        filter(Sub_Basin %in% input$sub_basin) %>%
        pull(GL_ID)
      selectizeInput(inputId = "lakeID", 
                     label = NULL,
                     choices = c("Please choose a lake ID" = "",cur_lakes),
                     multiple = TRUE)
    }
  })
  
  output$basinSelector <- renderUI({
    if(length(input$lakeID)==0) {
      selectizeInput(
        inputId = "sub_basin",
        label = NULL,
        choices = c("Please choose a sub basin" = "", lakes$Sub_Basin),
        multiple = TRUE)
    }
    else{
      cur_basins <- lakes %>%
        filter(GL_ID %in% input$lakeID) %>%
        pull(Sub_Basin)
      print(unique(cur_basins))
      selectizeInput(
        inputId = "sub_basin",
        label = NULL,
        choices = c("Please choose a sub basin" = "", cur_basins),
        multiple = TRUE)
    }
  })
  
  
}

shinyApp(ui, server)