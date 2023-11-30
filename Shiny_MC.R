library(needs)
needs(tidyverse, shiny, lubridate)

# LVL2_trees <- read_csv("MC_LVL2_trees.csv")
# FB1 <- LVL2_trees %>% 
#   filter(Tree == "FB1") %>% 
#   filter(Station == "S1")
# str(FB1)
#   
# LVL2_pasture <- read_csv("MC_LVL2_pasture.csv") %>% 
#   mutate(Station = "S0") %>% 
#   select(Tree = PastureID, Station, everything())
# FullTreeVec <- c("ET1", "ET2", "ET3", "ET4", "ET5", "ET6", "ET7", "ET8",
#                  "FB1", "FB2", "FB3", "FB4", "FB5", "FB6", "FB7", "FB8",
#                  "TV1", "TV2", "TV3", "TV4")
# PastureVec <- c("ETP1", "ETP2", "FBP1", "FBP2", "TVP")

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
                  max = ymd(as.character(Sys.Date())), 
                  value = c(ymd("2022-09-01"), ymd(as.character(Sys.Date())))),
      
      fluidRow(
        column(2,
               radioButtons("TreeID",
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
                 checkboxGroupInput("StationID",
                              label = h4("Select Station"),
                              choices = c("S0", "S1", "S2", "S3", "S4", "S5"),
                              selected = "S1"),
                 radioButtons("Variable",
                              label = h4("Select variable"),
                              choices = c("Solar", "Temp", "RH", "Atmos_pressure",
                                          "VPD", "Wetness", "Wind_direction",
                                          "Wind_speed", "Gust_speed", "Precipitation",
                                          "Precip_max"),
                              selected = "Solar"),
                 radioButtons("TimeRes",
                              label = h4("Select time resolution"),
                              choices = c("15 Min", "Hourly", "Daily", "Weekly"),
                              selected = "15 Min")))),
      titlePanel("Download options"),
      fluidRow(
        radioButtons("Time_format",
                     label = h4("Select time format"),
                     choices = list("ISO", "Excel_ready"),
                     selected = "ISO")),
      downloadButton("downloadData", "Download")
    ),
    
    mainPanel(plotOutput("plot",
                         hover = "plot_hover",
                         brush = "plot_brush"),
              verbatimTextOutput("plot_hoverinfo"),
              plotOutput("plot_brushedpoints"))
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # dataInput <- reactive({
  #   if(input$TreeID %in% FullTreeVec){
  #     LVL2_trees %>%
  #       filter(Tree == input$TreeID) %>%
  #       filter(Station == input$StationID) %>%
  #       filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
  #                Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>%
  #       pivot_longer(4:13, names_to = "Variable", values_to = "Measure")
  #   } else if(input$TreeID %in% PastureVec){
  #     LVL2_pasture %>%
  #       filter(Tree == input$TreeID) %>%
  #       filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
  #                Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>%
  #       pivot_longer(4:15, names_to = "Variable", values_to = "Measure")
  #   }
  # })
  dataInput <- reactive({
    read_csv(file.path("Microclimate_data_L3",
                     str_c(input$TreeID, "_MC_L3.csv")),
           show_col_types = F)
  })
  
  
  dataInput2 <- reactive({
    dataInput() %>% 
      filter(Tree == input$TreeID) %>%
            filter(Station == input$StationID) %>%
            filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
                     Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>%
            pivot_longer(4:ncol(dataInput()), 
                         names_to = "Variable", values_to = "Measure") %>% 
      filter(Variable == input$Variable)
  })
  
  dataInput3 <- reactive({
    if(input$TimeRes == "15 Min"){
      dataInput2()
    } else if(input$TimeRes == "Hourly"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "hour"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T)))
    } else if(input$TimeRes == "Daily"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "day"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T)))
    } else if(input$TimeRes == "Weekly"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "week"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T)))}
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
      ggtitle(str_c(input$TreeID, "_", input$StationID, "_", input$Variable))}) 
  
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
    if(input$Time_format == "ISO"){
      dataInput3()
    } else if(input$Time_format == "Excel_ready"){
      dataInput3() %>%
        mutate(Timestamp = as.character(Timestamp))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$TreeID, input$StationID, input$Variable,
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

# # Deploy app --------------------------------------------------------------
# !Don't do this! Maybe need to run the token line, not sure. But deploy by pressing the blue button at the top right

# library(rsconnect)
# 
# rsconnect::setAccountInfo(name='tmcfdata', token='1C78F1777CCC2DA19A8189627F5ECB3F', secret='RZhSSv4Yv9EYOPxkUlSy1tT6+828LquoaStTs1Qz')
# 
# deployApp()

# Test server -------------------------------------------------------------

testServer(server, {
  session$setInputs(TreeID = "FB1")
  session$setInputs(StationID = "S1")
  session$setInputs(Variable = "Solar")
  session$setInputs(dates = c(min = ymd("2022-09-01"),
                              max = ymd("2022-12-01")))
  session$setInputs(TimeRes = "15 min")
  print(dataInput3())
})

