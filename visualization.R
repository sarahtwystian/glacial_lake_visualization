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



ui <- fluidPage(
  sidebarPanel(
    sliderInput("area", "Area:", min = 0, max = max(lakes$area), value = c(0,max(lakes$area)), step=0.001),
    uiOutput("lakeSelector"),
    uiOutput("basinSelector"),
    #uiOutput("trendSelector"),
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
    
    if (nrow(y) == 0) {
      lakes$brush<-rep(FALSE, nrow(lakes))
    }
    else{
      lakes$sel<-rep(FALSE, nrow(lakes))
      y$selected_<-rep(TRUE, nrow(y))
      lakes<-left_join(lakes,y,by=c("GL_ID","time","area","Sub_Basin")) %>% 
        mutate(brush = ifelse(is.na(selected_), sel, selected_)) %>% 
        select(GL_ID, time, area, Sub_Basin, brush)
    }
    #print(y)
    #print(head(lakes))
    print("subbasin")
    print(length(input$sub_basin)==0)
    print("lakeid")
    print(length(input$lakeID)==0)
    
    if (length(input$sub_basin)!=0 & length(input$lakeID)==0){
      print("1")
      ggplot(data=filter(lakes, Sub_Basin %in% input$sub_basin), aes(time, area)) +
        geom_point(aes(color=brush), size = 1) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
    else if(length(input$lakeID)!=0 & length(input$sub_basin)==0){
      print("2")
      ggplot(data=filter(lakes, GL_ID %in% input$lakeID), aes(time, area)) +
        geom_point(aes(color=brush), size = 1) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
    else if(length(input$sub_basin)!=0 & length(input$lakeID)!=0){
      print("3")
      ggplot(data=filter(lakes, (GL_ID %in% input$lakeID) & (Sub_Basin %in% input$sub_basin)), aes(time, area)) +
        geom_point(aes(color=brush), size = 1) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
    else{
      print("4")
      ggplot(data=lakes, aes(time, area))+
        geom_point(aes(color=brush),  size = 1, show.legend = FALSE) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10() +
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    } 
    
  },
  res = 96, height = 250)
  

  
  selected <- reactive({
    #df<-filter(lakes,(Sub_Basin %in% input$sub_basin)&(GL_ID %in% input$lakeID))
    df<-if(length(input$sub_basin)!=0){filter(lakes, Sub_Basin %in% input$sub_basin) }else{lakes}
    df<-if(length(input$lakeID)!=0){filter(df, GL_ID %in% input$lakeID)}else{lakes}
    if(length(input$sub_basin)!=0|length(input$lakeID)!=0){
      brushedPoints(df, input$plot_brush, "time", "area", allRows = TRUE) %>%
        select(GL_ID, time, area, Sub_Basin, selected_) %>% 
        filter(selected_==TRUE)
    }
    else{
      brushedPoints(lakes, input$plot_brush, "time", "area", allRows = TRUE) %>%
        select(GL_ID, time, area, Sub_Basin, selected_) %>% 
        filter(selected_==TRUE)
    }
  })
  
  observeEvent(input$plot_reset, {
    selected(rep(FALSE, nrow(lakes)))
  })
  
  output$table = DT::renderDataTable({
    x <- selected() %>%
      select(GL_ID,time) %>% 
      top_n(1e3)
    print(x)
    
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
  
  # output$trendSelector <- renderUI({
  #   
  # })
  
  
}

shinyApp(ui, server)