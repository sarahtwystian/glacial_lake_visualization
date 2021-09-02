library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(dplyr)
library(readr)

lakes <- read_csv("lake_areas.csv")

ui <- fluidPage(
  sidebarPanel(
    sliderInput("area", "Area:", min = 0, max = max(lakes$area), value = c(0,max(lakes$area)), step=0.001),
    selectInput("lakeID", "Lake ID", choices = lakes$GL_ID, selected=NULL),
    width=12
  ),
  
  plotOutput("plot", brush = "plot_brush", height = 500),
  h4("lakeID"),
  uiOutput("images")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
       ggplot(lakes,aes(time, area, group=GL_ID))+
         geom_point( size = 1) +
         geom_line(aes(group = GL_ID), size = 0.7, alpha = 0.5) +
         scale_y_log10() +
         ylim(input$area[1],input$area[2]) +
         xlab("Year")
  },
  res = 96, height = 250)
  
  # get lakes and times for selected lakes
  selected <- reactive({
    brushedPoints(lakes, input$plot_brush, "time", "area") %>%
      select(GL_ID, time)
  })
  
  output$images <- renderUI({
    x <- selected() %>%
      top_n(1e3)
    if (nrow(x) == 0) {
      return()
    }
  
    base <- "https://glaciersblob.blob.core.windows.net/lakes/thumbnails/sentinel/"
    ids <- distinct(x, GL_ID) %>%
      pull(GL_ID)
    
    divs <- map(ids, ~ tagList(tags$div(class=.)))
    names(divs) <- ids
    
    for (i in seq_len(nrow(x))) {
      gl_id <- x$GL_ID[i]
      link <- str_c(base, gl_id, "_", x$time[i], ".png")
      divs[[gl_id]] <- tagAppendChild(divs[[gl_id]], tags$img(src=link, width=80))
      print(i)
      
    }
    divs
  })
}

shinyApp(ui, server)
