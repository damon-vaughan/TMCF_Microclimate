library(needs)
needs(tidyverse, shiny, lubridate)

import.log <- read_csv(file.path("Microclimate_data_supporting",
                                 "zl6_import_log.csv"), show_col_types = F)
max.date = max(import.log$Last.import, na.rm = T)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Microclimate data portal"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Cool stuff you can do:"),
      helpText("Move slider to select date range"),
      helpText("Hover pointer over plot to display the exact timestamp"),
      helpText("Zoom plots by clicking and dragging a square over the desired region"),
      helpText("Download data to a folder on your computer"),
      
      sliderInput("dates", 
                  label = h4("Select date range"), 
                  min = ymd("2022-09-01"), 
                  max = as_date(max.date), 
                  value = c(ymd("2022-09-01"), max.date)),
      
      fluidRow(
        column(2,
               radioButtons("Tree.view",
                            label = h4("Select tree"),
                            choices = c("ET1", "ET2", "ET3", "ET4",
                                        "ET5", "ET6", "ET7", "ET8",
                                        "FB1", "FB2", "FB3", "FB4",
                                        "FB5", "FB6", "FB7", "FB8",
                                        "TV1", "TV2", "TV3", "TV4",
                                        "ETP1", "ETP2", "FBP1", "FBP2", "TVP"),
                            selected = "ET1")),
        column(2, offset = 1,
               fluidRow(
                 checkboxGroupInput("Station.view",
                              label = h4("Select Station"),
                              choices = c("S0", "S1", "S2", "S3", 
                                          "S4", "S5", "Cansoil"),
                              selected = c("S0", "S1")),
                 radioButtons("Variable",
                              label = h4("Select variable"),
                              choices = c("Solar", "Temp", "RH", "Atmos_pressure",
                                          "VPD", "Wetness", "Wind_direction",
                                          "Wind_speed", "Gust_speed", "Precipitation",
                                          "Precip_max", "EpiMoisture", "EpiTemp"),
                              selected = "Solar"),
                 radioButtons("Time.res",
                              label = h4("Select time resolution"),
                              choices = c("15 Min", "Hourly", "Daily", "Weekly"),
                              selected = "15 Min")))),
      titlePanel("Download options"),
      fluidRow(
        radioButtons("Time.format",
                     label = h4("Select time format"),
                     choices = list("ISO", "Excel_ready"),
                     selected = "ISO")),
      downloadButton("downloadData", "Download")
    ),
    
    mainPanel(
      "Graph shows data until:",
      verbatimTextOutput("max.date.out"),
      plotOutput("plot",
                         hover = "plot_hover",
                         brush = "plot_brush"),
              verbatimTextOutput("plot_hoverinfo"),
              plotOutput("plot_brushedpoints"))
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # dataInput <- reactive({
  #   if(input$Tree.view %in% FullTreeVec){
  #     LVL2_trees %>%
  #       filter(Tree == input$Tree.view) %>%
  #       filter(Station == input$Station.view) %>%
  #       filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
  #                Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>%
  #       pivot_longer(4:13, names_to = "Variable", values_to = "Measure")
  #   } else if(input$Tree.view %in% PastureVec){
  #     LVL2_pasture %>%
  #       filter(Tree == input$Tree.view) %>%
  #       filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
  #                Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>%
  #       pivot_longer(4:15, names_to = "Variable", values_to = "Measure")
  #   }
  # })
  dataInput <- reactive({
    read_csv(file.path("Microclimate_data_L3",
                     str_c(input$Tree.view, "_MC_L3.csv")),
           show_col_types = F)
  })
  
  
  dataInput2 <- reactive({
    dataInput() %>% 
      filter(Tree == input$Tree.view) %>%
            filter(Station == input$Station.view) %>%
            filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
                     Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>%
            pivot_longer(4:ncol(dataInput()), 
                         names_to = "Variable", values_to = "Measure") %>% 
      filter(Variable == input$Variable)
  })
  
  dataInput3 <- reactive({
    if(input$Time.res == "15 Min"){
      dataInput2()
    } else if(input$Time.res == "Hourly"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "hour"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T)))
    } else if(input$Time.res == "Daily"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "day"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T)))
    } else if(input$Time.res == "Weekly"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "week"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T)))}
  })
  
  output$max.date.out <- renderText({
    as.character(max(dataInput3()$Timestamp))
  })
  
  output$plot <- renderPlot({
    ggplot(dataInput3()) +
      geom_line(aes(x = Timestamp, y = Measure, color = Station)) +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$Tree.view, "_", input$Station.view, "_", input$Variable))}) 
  
  output$plot_hoverinfo <- renderPrint({
    val <- nearPoints(dataInput2(), input$plot_hover, maxpoints = 1)
    unique(val$Timestamp)
  })
  
  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput2(), input$plot_brush)
    if (nrow(dat) == 0)
      return()
    ggplot(dat) +
      geom_line(aes(x = Timestamp, y = Measure)) +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) 
  })
  
  dataDL <- reactive({
    if(input$Time.format == "ISO"){
      dataInput3()
    } else if(input$Time.format == "Excel_ready"){
      dataInput3() %>%
        mutate(Timestamp = as.character(Timestamp))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$Tree.view, input$Station.view, input$Variable,
                  input$dates[1], input$dates[2], sep = "_"),
            ".csv")
    },
    content <-  function(file) {
      write_csv(dataDL(), file)
    }
  )
}

# Run app ----------------------------------------------------------------

shinyApp(ui, server)

# Test server -------------------------------------------------------------

# testServer(server, {
#   session$setInputs(Tree.view = "FB1")
#   session$setInputs(Station.view = "S1")
#   session$setInputs(Variable = "Solar")
#   session$setInputs(dates = c(min = ymd("2022-09-01"),
#                               max = ymd("2022-12-01")))
#   session$setInputs(Time.res = "15 min")
#   print(dataInput3())
# })

