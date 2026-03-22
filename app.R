library(shiny)
library(bslib)
library(DT)
library(rsconnect)

# Define UI for app that draws a histogram ----
ui <- page_navbar(
  # App title ----
  title = "Logistics Company",
  sidebar = sidebar(
    width = "400px",
    h4("General Information"),
    card(
      class = "p-2 mb-2 bg-primary text-white",
      strong("Average Detention Time"),
      div(textOutput("avgDetentionValue"), class = "fs-5 fw-bold")
    ),
    card(
      class = "p-2 mb-2 bg-danger text-white",
      strong("Max Detention Time"),
      div(textOutput("maxDetention"), class = "fs-5 fw-bold")
    ),
    p(strong("Most Popular State:"), textOutput("topState", inline = TRUE)),
    p(strong("Most Popular City:"), textOutput("topCity", inline = TRUE)),
    p(strong("Most Popular Delivery Type:"), textOutput("topDeliveryType", inline = TRUE)),
    
  ),
  nav_panel(
    title = "Detention Statistics",
    # layout_columns(
    #   col_widths = c(6, 6),  # 50% / 50%
    #   
    #   value_box(
    #     title = "Average Detention Time",
    #     value = div(
    #       textOutput("avgDetentionValue"),
    #       # style = "font-size: 1.25rem; font-weight: bold;margin: 0px; "
    #     ),
    #     theme = "primary",
    #   ),
    #   
    #   value_box(
    #     title = "Max Detention Time",
    #     value = div(
    #       textOutput("maxDetention"),
    #       # style = "font-size: 1.25rem; font-weight: bold; margin: 0px;"
    #     ),
    #     theme = "danger"
    #   )
    # ),
    card(
      plotOutput(outputId = "hist"),  
    ),
    card(
      plotOutput("pieChart"),
      )
      
    
  ),
  nav_panel(
    title = "Data Table",
    DTOutput("rawTable")
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  data <- read.csv("data/delivery_events_result.csv", stringsAsFactors = FALSE)

  output$hist <- renderPlot({
    hist(data$detention_minutes,
      breaks = 50,
     main = "Detention Time Distribution",
     xlab = "Detention Minutes",
     col = "lightblue",
     border = "white")
    axis(1, at = seq(0, 300, by = 10))
  })
  
  output$pieChart <- renderPlot({
    filtered_data <- data[!is.na(data$on_time_flag) & data$on_time_flag != "", ]
    
    delivery_counts <- table(filtered_data$on_time_flag)
    
    pct <- round(delivery_counts / sum(delivery_counts) * 100, 1)
    
    pie(delivery_counts,
        labels = paste(names(delivery_counts), pct, "%"),
        col = c("red", "blue"),
        main = "On-Time Delivery")
  })
  output$avgDetentionValue <- renderText({
    avg_time <- mean(data$detention_minutes[!is.na(data$detention_minutes)], na.rm = TRUE)
    paste(round(avg_time, 1), "minutes")
  })
  output$topState <- renderText({
    tbl <- sort(table(data$location_state), decreasing = TRUE)
    names(tbl)[1]
  })
  output$topCity <- renderText({
    tbl <- sort(table(data$location_city), decreasing = TRUE)
    names(tbl)[1]
  })
  output$topDeliveryType <- renderText({
    tbl <- sort(table(data$event_type), decreasing = TRUE)
    names(tbl)[1]
  })
  output$maxDetention <- renderText({
    max_time <- max(data$detention_minutes, na.rm = TRUE)
    paste(max_time, "minutes")
  })
  output$rawTable <- renderDT({
    datatable(data)
  })
  
}

shinyApp(ui = ui, server = server)
