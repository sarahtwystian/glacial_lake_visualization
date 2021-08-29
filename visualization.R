library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")
library("dplyr")

lakes<-read.csv("links.csv")

graph<-ggplotly(
  ggplot(lakes,aes(x=year,y=Area, group=GL_ID))+
    geom_line()+
    geom_point()+
    scale_x_continuous(breaks=seq(2015,2021,1),limits=c(2015,2021))+
    scale_y_continuous(breaks=seq(0,5,0.2))+
    xlab("Year")
  )


ui <- fluidPage(
  sidebarPanel(
    sliderInput("area", "Area:", min = 0, max = 5, value = c(0,5), step=0.001),
    selectInput("lakeID", "Lake ID", choices = lakes$GL_ID, selected=NULL),
    width=12
  ),
  
  plotOutput("plot", brush = "plot_brush"),
  h4(lakeID),
  uiOutput("img")
  
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
     #if (is.null(input$lakeID)){
       ggplot(lakes,aes(x=year,y=Area, group=GL_ID))+
         geom_line()+
         geom_point()+
         scale_x_continuous(breaks=seq(2015,2021,1),limits=c(2015,2021))+
         ylim(input$area[1],input$area[2])+
         xlab("Year")
     #}
    # else {
    #   #lakes_grouped %>% 
    #   ggplot(lakes,aes(x=year, y=Area, group=input$lakeID))+
    #     geom_line()+
    #     geom_point()+
    #     scale_x_continuous(breaks=seq(2015,2021,1),limits=c(2015,2021))+
    #     ylim(input$area[1],input$area[2])+
    #     xlab("Year")
    # }
      }, 
    res = 96)
  
  # output$img <- renderUI({
  #   tags$img(src = "https://glaciersblob.blob.core.windows.net/lakes/thumbnails/sentinel/GL080178E30564N_2020-12-07.png")
  # })
  
 
}


shinyApp(ui, server)

