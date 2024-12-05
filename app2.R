library(needs)
needs(tidyverse, shiny, lubridate)

# Shiny convention: Input terms always lower case and one word if possible

source("app_functions_MC.R")
options(readr.show_col_types = FALSE)

import.log <- read_csv(file.path("Microclimate_data_supporting",
                                 "zl6_import_log.csv"))
max.date <- max(import.log$Last.import, na.rm = T)

# Read in maintenance actions so they can be applied to the plot
MC.actions <- read_csv(file.path("Microclimate_data_supporting",
                                 "MC_maintenance_actions.csv"))

zl6 <- read_csv(file.path("Microclimate_data_supporting",
                          "zl6_database_long.csv"))

instruments <- read_csv(file.path("Microclimate_data_supporting", "Meter_instruments.csv"))

# MC actions: Make sheet ---------------------------------------------------

tree.vec <- full.tree.vec
needs(readxl)

maintenance.dir <- "C:/Users/User/OneDrive - University of Kentucky/TMCF/Continuous_data/Maintenance_notes"

v.names <- read_excel(file.path("Microclimate_data_supporting",
                                "Variable_names.xlsx")) %>% 
  select(Variable = Final, Instrument, Label.loc) %>% 
  filter(!is.na(Instrument))

# x <- "FB1"
read_sheet <- function(x){
  sheet <- read_excel(file.path(maintenance.dir, "SensorNotes_All.xlsx"), 
                      sheet = x) %>%
    slice(14:30) %>% 
    mutate(Tree = x) %>%
    select(Tree, Location, Sensor = Part, everything()) %>% 
    filter(!is.na(Location))
  sheet2 <- sheet %>%
    pivot_longer(4:ncol(sheet), names_to = "Date", values_to = "Action") %>% 
    filter(!is.na(Action))
  sheet3 <- sheet2 %>% 
    separate(Sensor, into = c("Station", "Instrument"), sep = " ")
  sheet4 <- sheet3 %>% 
    full_join(v.names, by = "Instrument", relationship = "many-to-many") %>% 
    filter(!is.na(Action))
  return(sheet4)
}
# read_sheet(full.tree.vec[15])

d <- lapply(full.tree.vec, read_sheet) %>%
  bind_rows() %>%
  na.omit()

d2 <- d %>%
  mutate(Date = str_sub(Date, start = 1, end = 10),
         ToD = "10:00:00",
         Timestamp = str_c(Date, ToD, sep = " ")) %>%
  select(Tree, Station, Variable, Timestamp, Action, Label.loc) %>%
  mutate(Timestamp = ymd_hms(Timestamp, tz = "UTC"))

write_csv(d2, file.path("Microclimate_data_supporting", "MC_maintenance_actions.csv"))

# Read in maintenance actions so they can be applied to the plot
MC.actions <- read_csv(file.path("Microclimate_data_supporting",
                                 "MC_maintenance_actions.csv"))

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
                 radioButtons("station",
                              label = h4("Select Station"),
                              choices = c("S0", "S1", "S2", "S3",
                                          "S4", "S5", "CS"),
                              selected = c("S1")),
                 radioButtons("variable",
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
                   radioButtons("level",
                                label = h4("Select level"),
                                choices = c("L3", "L4"),
                                selected = "L3")),
                 fluidRow(
                   radioButtons("maintenance",
                                label = h4("Show maintenance?"),
                                choices = c("yes", "no"),
                                selected = "yes")),
                 fluidRow(
                   radioButtons("time.res",
                                label = h4("Select time resolution"),
                                choices = c("15 Min", "Hourly", "Daily", "Weekly"),
                                selected = "15 Min")))),
      downloadButton("save.errors", "Save errors")
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
                 tableOutput("errors")),
        tabPanel("Wiring", tableOutput("wiring1"), 
                 tableOutput("wiring2"),
                 tableOutput("wiring3"), tableOutput("wiring4")),
        tabPanel("Data", tableOutput("data1"), tableOutput("data2")),
        tabPanel("Summary", tableOutput("summary"))),
      fluidRow(
        column(6,
               radioButtons( 
                 "instrument", 
                 label = h4("Apply to instrument?"), 
                 choices = c("Yes", "No"),
                 selected = "Yes")),
        column(6,
               radioButtons( 
                 "datalogger", 
                 label = h4("Apply to datalogger?"), 
                 choices = c("Yes", "No"),
                 selected = "No"))),
      fluidRow(
        textInput( 
          "text", 
          "Text input", 
          placeholder = "Enter note here"
        ),
      fluidRow(
        actionButton("add.bad.data", "Add to list")
      ),
      fluidRow(
        actionButton("remove.last.row", "Remove last row")
      ),
      fluidRow(
        plotOutput("plot_brushedpoints")
      )))
  )
)

# Server ---------------------------------------------------------

server <- function(input, output, session) {
  
  dataInput <- reactive({
    read_csv(file.path(str_c("Microclimate_data_", input$level), 
                             str_c(input$tree, "_MC_", input$level, ".csv"))) %>% 
      filter(Tree == input$tree) %>%
      filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC"))
  })
  
  dataInput2 <- reactive({
    dataInput() %>% 
      filter(Station %in% input$station) %>%
      pivot_longer(4:ncol(dataInput()), 
                   names_to = "Variable", values_to = "Measure") %>% 
      filter(Variable == input$variable)
  })
  
  labelInput <- reactive({
    MC.actions %>%
      filter(Tree == input$tree) %>%
      filter(Station == input$station) %>%
      filter(Variable == input$variable) %>% 
      filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC"))
  })

## Plots -------------------------------------------------------------
  
  output$plot <- renderPlot({
    p <- ggplot(dataInput2()) +
      geom_line(aes(x = Timestamp, y = Measure, color = Station)) +
      geom_vline(aes(xintercept = as_datetime("2024-10-01 00:00:00")), linewidth = 1) +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$tree, "_", input$variable))
    
    if(input$maintenance == "yes"){
      p <- p +
        geom_label(data = labelInput(), aes(x = Timestamp, y = Label.loc, label = Action))
    }
    p
    }) 
  
  output$plot_clickinfo <- renderPrint({
    val <- nearPoints(dataInput2(), input$plot_click, maxpoints = 1)
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
  
## Errors ---------------------------------------------------------

  error.entry <- reactive({
    dat <- brushedPoints(dataInput2(), input$plot_brush)
    dat2 <- tibble(Tree = input$tree,
                   Station = input$station,
                   Variable = input$variable,
                   Start = min(dat$Timestamp, na.rm = T),
                   End = max(dat$Timestamp, na.rm = T),
                   Apply.to.instrument = input$instrument,
                   Apply.to.datalogger = input$datalogger,
                   Note = input$text) %>% 
      mutate(Start = as.character(Start),
             End = as.character(End))
    return(dat2)
  })

  error.table <- reactiveVal(
    tibble(Tree = character(),
           Station = character(),
           Variable = character(),
           Start = character(), 
           End = character(),
           Apply.to.instrument = character(),
           Apply.to.datalogger = character(),
           Note = character()))
  
  observeEvent(input$add.bad.data, {
    error.table() %>%
      add_row(
        Tree = input$tree,
        Station = input$station,
        Variable = input$variable,
        Start = error.entry()$Start[1],
        End = error.entry()$End[1],
        Apply.to.instrument = error.entry()$Apply.to.instrument[1],
        Apply.to.datalogger = error.entry()$Apply.to.datalogger[1],
        Note = error.entry()$Note[1]) %>%
      error.table()
  })
  
  observeEvent(input$remove.last.row, {
    error.table() %>% 
      slice_head(n = -1) %>% 
      error.table()
  })
  
  output$errors <- renderTable(error.table())

## Data and tables ---------------------------------------------------

  output$max.date.out <- renderText({
    as.character(max(dataInput2()$Timestamp, na.rm = T))
  })
  
  locate.datalogger <- reactive({
    inst <- instruments %>%
      filter(Variable == input$variable) %>%
      pull(Instrument)
    zl6 %>%
      filter(Location == input$tree & Station %in% input$station &
               Instrument == inst) %>% 
      select(MC.ID) %>% 
      distinct()
  })
  
  output$wiring1 <- renderTable({
    instruments %>%
      filter(Variable == input$variable)
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
      filter(Variable == input$variable) %>% 
      pull(Instrument)
    instruments %>% 
      filter(Instrument == inst)
  })
  
  output$save.errors <- downloadHandler(
    filename = function() {
      str_c("errors_", Sys.time(), ".csv")
    },
    content <-  function(file) {
      write_csv(error.table(), file)
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
#   session$setInputs(variable = "Solar")
#   session$setInputs(daterange = c(min = ymd("2022-09-01"),
#                               max = ymd("2023-12-01")))
#   session$setInputs(time.res = "15 min")
#   test <<- print(dataInput2())
# })

