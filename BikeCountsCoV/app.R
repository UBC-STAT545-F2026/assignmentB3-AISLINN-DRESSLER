library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(tidyr)

#importing dataset:
bikeCount <- read.csv("bike-volume-data.csv")

# Parse the date with the correct format: "1-Aug-18" = "%d-%b-%y"
bikeCount$Date <- as.Date(bikeCount$Date, format = "%d-%b-%y")

# Define UI for application that draws a histogram
ui <- fluidPage(
  #title panel:
  titlePanel("Bike Count Data from Metro Vancouver"),

  #description:
  p("Explore how the amount of cyclists varies by location and date. The data is from the City of Vancouver automatic bike counters from August 2018 to July 2022."),

  sidebarLayout(
    sidebarPanel(
      #slider panel for selecting the date range:
      dateRangeInput(
        inputId = "selectedDateRange",
        label = "Choose a Date Range:",
        start = "2018-08-01",
        end = "2022-07-19",
        min = "2018-08-01",
        max = "2022-07-19",
        format = "yyyy-mm-dd"
      ),
      #date checkbox for location selection:
      checkboxGroupInput(
        inputId = "locationsSelected",
        label = "Select the Locations (default is all locations):",
        choices = c("Burrard.at.Cornwall","Canada.Line.Bridge.at.West.Kent","Comox.at.Thurlow","Dunsmuir.Viaduct","East.10th.at.Clark","Hornby.and.Robson","Lions.Gate.Bridge.at.Spirit.Trail","Ontario.and.11th","Point.Grey.at.Stephens","Point.Grey.at.Volunteer.Park","Seawall.2nd.Beach.Pool","Seawall.at.Creekside.Community.Centre","Seawall.at.David.Lam.Park","Seawall.at.Harbour.Green.Park","Seawall.at.HMS.Discovery","Seawall.at.Science.World","Smithe.at.Expo","Stephens.at.Point.Grey","Union.at.Hawks"),
        selected = NULL
      )

    ),
    mainPanel(
      navset_tab(
        nav_panel(title = "Plot", plotOutput("coolplot")),
        nav_panel(title = "Summary Table", tableOutput("results"))
      )
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #filter data:
  filtered_data <- reactive({
    # Convert input dates to Date objects
    start_date <- as.Date(input$selectedDateRange[1])
    end_date <- as.Date(input$selectedDateRange[2])

    data <- bikeCount %>%
      filter(Date >= start_date & Date <= end_date) %>%
      select(-Date) %>%
      summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Location", values_to = "Number of Cyclists Counted")

    # Filter by selected locations if any are selected
    if (!is.null(input$locationsSelected) && length(input$locationsSelected) > 0) {
      data <- data %>%
        filter(Location %in% input$locationsSelected)
    }

    data
  })

  #plot showing bar graph with volume at all sites
  output$coolplot <- renderPlot({
    data <- filtered_data()

    # Validation
    if(nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selected date range", cex = 1.5)
      return()
    }

    ggplot(data, aes(x = Location, y = `Number of Cyclists Counted`)) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Number of Cyclists Counted", x = "Location")
  })

  output$results <- renderTable({
    filtered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
