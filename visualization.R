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
library(shinyjs)


orig_lakes <- read_csv("lake_areas-unet-historical.csv")
t<-orig_lakes %>% 
  group_by(GL_ID) %>% 
  summarise(slope = lm(area ~ time)$coefficients['time'])
lakes<-orig_lakes %>% 
  left_join(t) %>% 
  mutate(slope = coalesce(slope, 0)) %>% 
  mutate(trend=ifelse(slope>0,"increase",ifelse(slope==0,"constant","decrease")))

ui <- fluidPage(
  sidebarPanel(
    useShinyjs(),
    sliderInput("area", "Area:", min = 0, max = max(lakes$area), value = c(0,max(lakes$area)), step=0.001),
    tabsetPanel(id = "tabset",
                tabPanel("Sub basin",
                         
                         selectizeInput(
                           inputId = "sub_basin",
                           label = "1. Sub Basin",
                           choices = c("Please choose some sub basins" = "", lakes$Sub_Basin),
                           multiple = TRUE),
                         #actionButton("reset_id1", "Reset"),
                         selectizeInput(inputId = "lakeID_1",
                                        label = "2. Lake ID",
                                        choices = c("Please choose some lake IDs" = "",lakes$GL_ID),
                                        multiple = TRUE),
                         actionButton("reset_sb", "Reset")
                         #uiOutput("lakeID_1")
                         
                ),
                tabPanel("Lake ID",
                         
                         selectizeInput(inputId = "lakeID_2",
                                        label = NULL,
                                        choices = c("Please choose some lake IDs" = "",lakes$GL_ID),
                                        multiple = TRUE),
                         actionButton("reset_id2", "Reset")
                         
                         
                ),
                tabPanel("Trend",
                         
                         selectizeInput(
                           inputId = "trend",
                           label = "1. Trend",
                           choices = c("Please choose the trend that you are interested in" = "", unique(lakes$trend)),
                           multiple = TRUE),
                         
                         selectizeInput(inputId = "lakeID_3",
                                        label = "2. Lake ID",
                                        choices = c("Please choose some lake IDs" = "",lakes$GL_ID),
                                        multiple = TRUE),
                         actionButton("reset_trend", "Reset")
                        
                         
                )

                #actionButton("go", "Plot")
                
    ),
    width=12
  ),
  
  mainPanel(  plotOutput("plot", brush = "plot_brush", height = 500, dblclick = "plot_reset"),
              DT::dataTableOutput('table'),
              width=12)
  
  
)

server <- function(input, output){
  id <- reactive({
    ########update selections
    if(length(input$sub_basin)!=0){
      filter(lakes, Sub_Basin %in% input$sub_basin)
    }
  })
  observeEvent(id(), {
    choices <- unique(id()$GL_ID)
    updateSelectizeInput(inputId = "lakeID_1", choices = c("Please choose some lake IDs" = "",choices))
  })
  
  id_3 <- reactive({
    ########update selections
    if(length(input$trend)!=0){
      filter(lakes, trend %in% input$trend)
    }
  })
  observeEvent(id_3(), {
    choices <- unique(id_3()$GL_ID)
    updateSelectizeInput(inputId = "lakeID_3", choices = c("Please choose some lake IDs" = "",choices))
  })
  
  
  
  
  #####clear selections
  observeEvent(input$reset_sb, {
    shinyjs::reset("sub_basin")
    shinyjs::reset("lakeID_1")
    shinyjs::reset("plot")
  })
  
  observeEvent(input$reset_id2, {
    shinyjs::reset("lakeID_2")
    shinyjs::reset("plot")
    
  })
  
  observeEvent(input$reset_trend, {
    shinyjs::reset("trend")
    shinyjs::reset("lakeID_3")
    shinyjs::reset("plot")
    
  })
  
  
  ######

  selected <- reactive({
    if (input$tabset == "Sub basin"){
      if(length(input$sub_basin)!=0&length(input$lakeID_1)==0){
        df<-filter(lakes,Sub_Basin%in%input$sub_basin)
      }else if(length(input$sub_basin)!=0&length(input$lakeID_1)!=0){
        df<-filter(lakes, (GL_ID %in% input$lakeID_1) & (Sub_Basin %in% input$sub_basin))
      }else{
        df<-lakes
      }
      brushedPoints(df, input$plot_brush, "time", "area", allRows = TRUE) %>%
        dplyr::select(GL_ID, time, area, Sub_Basin, selected_) %>%
        filter(selected_==TRUE)
      
    }
    else if (input$tabset == "Trend"){
      print(input$trend)
      if(length(input$trend)!=0&length(input$lakeID_3)==0){
        df<-filter(lakes,trend%in%input$trend)
      }else if(length(input$trend)!=0&length(input$lakeID_3)!=0){
        df<-filter(lakes, (GL_ID %in% input$lakeID_3) & (trend %in% input$trend))
      }else{
        df<-lakes
      }
      brushedPoints(df, input$plot_brush, "time", "area", allRows = TRUE) %>%
        dplyr::select(GL_ID, time, area, Sub_Basin, selected_) %>%
        filter(selected_==TRUE)
      
    }
    else{
      if(length(input$lakeID_2)!=0){
        df<-filter(lakes, GL_ID %in% input$lakeID_2)
      }else{
        df<-lakes
      }
      brushedPoints(df, input$plot_brush, "time", "area", allRows = TRUE) %>%
        dplyr::select(GL_ID, time, area, Sub_Basin, selected_) %>%
        filter(selected_==TRUE)
    }
    
  })
  
  output$plot <- renderPlot({
    y <- selected() %>%
      top_n(1e3)
    
    if (nrow(y) == 0) {
      lakes$brush<-rep(FALSE, nrow(lakes))
    }
    else{
      lakes$sel<-rep(FALSE, nrow(lakes))
      y$selected_<-rep(TRUE, nrow(y))
      lakes<-left_join(lakes,y,by=c("GL_ID","time","area","Sub_Basin")) %>% 
        mutate(brush = ifelse(is.na(selected_), sel, selected_)) %>% 
        dplyr::select(GL_ID, time, area, Sub_Basin, brush)
    }
    
    
    if (length(input$sub_basin)!=0 & length(input$lakeID_1)==0){
      
      ggplot(data=filter(lakes, Sub_Basin %in% input$sub_basin), aes(time, area)) +
        geom_point(aes(color=brush), size = 1, show.legend = FALSE) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
    else if(length(input$sub_basin)!=0 & length(input$lakeID_1)!=0){
      ggplot(data=filter(lakes, (GL_ID %in% input$lakeID_1) & (Sub_Basin %in% input$sub_basin)), aes(time, area)) +
        geom_point(aes(color=brush), size = 1, show.legend = FALSE) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
    
    
    else if(length(input$lakeID_2)!=0){
      
      ggplot(data=filter(lakes, GL_ID %in% input$lakeID_2), aes(time, area)) +
        geom_point(aes(color=brush), size = 1, show.legend = FALSE) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
    
    else if (length(input$trend)!=0 & length(input$lakeID_3)==0){
      
      ggplot(data=filter(lakes, trend %in% input$trend), aes(time, area)) +
        geom_point(aes(color=brush), size = 1, show.legend = FALSE) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
    else if(length(input$trend)!=0 & length(input$lakeID_3)!=0){
      ggplot(data=filter(lakes, (GL_ID %in% input$lakeID_3) & (trend %in% input$trend)), aes(time, area)) +
        geom_point(aes(color=brush), size = 1, show.legend = FALSE) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10()+
        ylim(input$area[1],input$area[2]) +  id <- reactive({
          ########update selections
          if(length(input$sub_basin)!=0){
            filter(lakes, Sub_Basin %in% input$sub_basin)
          }
        })
        observeEvent(id(), {
          choices <- unique(id()$GL_ID)
          updateSelectizeInput(inputId = "lakeID_1", choices = c("Please choose some lake IDs" = "",choices))
        })
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
    
    else{
      ggplot(data=lakes, aes(time, area))+
        geom_point(aes(color=brush),  size = 1, show.legend = FALSE) +
        geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
        scale_y_log10() +
        ylim(input$area[1],input$area[2]) +
        xlab("Year")+
        scale_color_manual(values=c("black","red"))
    }
  })
  
  
  output$table = DT::renderDataTable({
    x <- selected() %>%
      dplyr::select(GL_ID,time) %>% 
      top_n(1e3)
    
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
  
  
}

shinyApp(ui, server)