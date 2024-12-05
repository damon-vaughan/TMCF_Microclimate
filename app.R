library(needs)
needs(tidyverse, shiny, lubridate)

source("app_functions_MC.R")
options(readr.show_col_types = FALSE)

import.log <- read_csv(file.path("Microclimate_data_supporting",
                                 "zl6_import_log.csv"))
max.date <- max(import.log$Last.import, na.rm = T)

zl6 <- read_csv(file.path("Microclimate_data_supporting",
                          "zl6_database_long.csv"))

instruments <- read_csv(file.path("Microclimate_data_supporting", "Meter_instruments.csv"))


# UI --------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Microclimate data portal"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("daterange", 
                  label = h4("Select date range"), 
                  min = ymd("2022-06-01"), 
                  max = as_date(Sys.time()), 
                  value = c(ymd("2022-06-01"), Sys.time())),
      fluidRow(radioButtons("fixed.y", inline = T,
                            label = h4("Fix y-axis?"),
                            choices = c("No", "Coarse", "Fine"),
                            selected = "No")),
      fluidRow(sliderInput("y.coarse",
                           label = h4("Select coarse y-axis range"),
                           min = 0,
                           max = 1500,
                           value = c(0, 1500))),
      fluidRow(
        column(6,
               sliderInput("y.fine.min",
                           label = h4("Select fine y-axis min"),
                           min = 0,
                           max = 100,
                           value = 0,
                           step = 0.5)),
        column(6,
               sliderInput("y.fine.range",
                           label = h4("Select fine y-axis range"),
                           min = 0,
                           max = 20,
                           value = 0,
                           step = 0.5))),
      fluidRow(
        column(3,
               radioButtons("tree",
                            label = h4("Select tree"),
                            choices = c("ET1", "ET2", "ET3", "ET4",
                                        "ET5", "ET6", "ET7", "ET8",
                                        "FB1", "FB2", "FB3", "FB4",
                                        "FB5", "FB6", "FB7", "FB8",
                                        "TV1", "TV2", "TV3", "TV4",
                                        "ETP1", "ETP2", "FBP1", "FBP2", "TVP"),
                            selected = "ET1")),
        column(3, offset = 1,
               fluidRow(
                 checkboxGroupInput("station",
                              label = h4("Select Station"),
                              choices = c("S0", "S1", "S2", "S3",
                                          "S4", "S5", "CS"),
                              selected = c("S0", "S1", "S2", "S3",
                                           "S4", "S5", "CS")),
                 # radioButtons("station",
                 #              label = h4("Select Station"),
                 #              choices = c("S0", "S1", "S2", "S3", 
                 #                          "S4", "S5", "CS"),
                 #              selected = c("S1")),
                 radioButtons("Variable",
                              label = h4("Select variable"),
                              choices = c("Solar", "Temp", "RH", "Atmos_pressure",
                                          "VPD", "LWS_Count", "Wind_direction",
                                          "Wind_speed", "Gust_speed", "Precipitation",
                                          "Precip_max", "EpiMoisture", "EpiTemp",
                                          "ECRN-100_Precipitation",
                                          "ECRN-100_Precip_max"),
                              selected = "Solar"),
                 )),
          column(3, offset = 1,
                 fluidRow(
                   radioButtons("Level",
                                label = h4("Select level"),
                                choices = c("L3", "L4"),
                                selected = "L4")),
                 fluidRow(
                   radioButtons("Time.res",
                                label = h4("Select time resolution"),
                                choices = c("15 Min", "Hourly", "Daily", "Weekly"),
                                selected = "15 Min")))),
      titlePanel("Download options"),
      fluidRow(
        column(5,
               radioButtons("Time.format",
                            label = h4("Select time format"),
                            choices = list("ISO", "Excel_ready"),
                            selected = "ISO")),
        column(5,
               radioButtons("All.variables",
                            label = h4("Download all variables?"),
                            choices = list("All", "Only_selected"),
                            selected = "Only_selected"))
               ),
      downloadButton("downloadData", "Download")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
                 "Graph shows data until:",
                 verbatimTextOutput("max.date.out"),
                 plotOutput("plot",
                            click = "plot_click",
                            brush = "plot_brush"),
                 verbatimTextOutput("plot_clickinfo"),
                 plotOutput("plot_brushedpoints")),
        tabPanel("Wiring", tableOutput("wiring1"), tableOutput("wiring2"),
                 tableOutput("wiring3"), tableOutput("wiring4")),
        tabPanel("Data", tableOutput("data1"), tableOutput("data2")),
        tabPanel("Summary", tableOutput("summary"))))
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  dataInput <- reactive({
    read_csv(file.path(str_c("Microclimate_data_", input$Level), 
                             str_c(input$tree, "_MC_", input$Level, ".csv"))) %>% 
      filter(Tree == input$tree) %>%
      filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC"))
  })
  
  dataInput2 <- reactive({
    dataInput() %>% 
      filter(Station %in% input$station) %>%
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
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>% 
        ungroup()
    } else if(input$Time.res == "Daily"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "day"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>% 
        ungroup()
    } else if(input$Time.res == "Weekly"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "week"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>% 
        ungroup()}
  })
  
  

## Plots -------------------------------------------------------------------
  
  output$plot <- renderPlot({
    p <- ggplot(dataInput3()) +
      geom_line(aes(x = Timestamp, y = Measure, color = Station)) +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$tree, "_", input$Variable))
    
    if(input$fixed.y == "Coarse"){
      p <- p +
        ylim(input$y.coarse[1], input$y.coarse[2])
    }
    
    if(input$fixed.y == "Fine"){
      p <- p +
        ylim(input$y.fine.min, input$y.fine.min + input$y.fine.range)
    }
    
    p
    }) 
  
  output$plot_clickinfo <- renderPrint({
    val <- nearPoints(dataInput3(), input$plot_click, maxpoints = 1)
    unique(val$Timestamp)
  })
  
  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput3(), input$plot_brush)
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

## Data and tables --------------------------------------------------------

  output$max.date.out <- renderText({
    as.character(max(dataInput3()$Timestamp, na.rm = T))
  })
  
  locate.datalogger <- reactive({
    inst <- instruments %>%
      filter(Variable == input$Variable) %>%
      pull(Instrument)
    zl6 %>%
      filter(Location == input$tree & Station %in% input$station &
               Instrument == inst) %>% 
      select(MC.ID) %>% 
      distinct()
  })
  
  output$wiring1 <- renderTable({
    instruments %>%
      filter(Variable == input$Variable)
  })
 
  output$wiring2 <- renderTable({
    locate.datalogger() 
  })
   
  output$wiring3 <- renderTable({
    zl6 %>%
      filter(Location == input$tree & MC.ID %in% locate.datalogger()$MC.ID) %>% 
      select(-ZL.ID, -Location) %>% 
      mutate(Port = as.numeric(Port))
  })
  
  output$wiring4 <- renderTable({
    inst <- instruments %>%
      filter(Variable == input$Variable) %>% 
      pull(Instrument)
    instruments %>% 
      filter(Instrument == inst)
  })
  
  data.for.summaries <- reactive({
    dataInput3() %>% 
      mutate(Timestamp = as.character(Timestamp)) 
  })
  
  output$data1 <- renderTable({
    head(data.for.summaries())
  })
  
  output$data2 <- renderTable({
    tail(data.for.summaries())
  })
  
  output$summary <- renderTable({
    summarise_MC(data.for.summaries())
  })

## Download options --------------------------------------------------------

  data.for.download <- reactive({
    if(input$All.variables == "All"){
      dataInput()
    } else {
      dataInput3()
    } 
    })
  
  data.for.download2 <- reactive({
    if(input$Time.format == "Excel_ready"){
      data.for.download() %>% 
        mutate(Timestamp = as.character(Timestamp))
    } else {
      data.for.download()
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$tree,
                  str_sub(as.character(input$daterange[1]), start = 1, end = 10), 
                  str_sub(as.character(input$daterange[2]), start = 1, end = 10), 
                  sep = "_"),
            ".csv")
    },
    content <-  function(file) {
      write_csv(data.for.download2(), file)
    }
  )
}

# Run app ------------------------------------------------------------

shinyApp(ui, server)

# Test server -------------------------------------------------------

# testServer(server, {
#   session$setInputs(tree = "ET1")
#   session$setInputs(station = c(
#     # "S0", 
#     "S1"
#     # , 
#     # "S2", "S3",
#                                 # "S4"
#     # "S5", "Cansoil"
#     ))
#   session$setInputs(Variable = "Solar")
#   session$setInputs(daterange = c(min = ymd("2022-09-01"),
#                               max = ymd("2023-12-01")))
#   session$setInputs(Time.res = "15 min")
#   test <<- print(dataInput3())
# })

