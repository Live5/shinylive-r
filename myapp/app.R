library(shiny)
library(shinydashboard)
library(plotly)
library(dygraphs)
library(leaflet)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Fancy Shiny App",
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tab 1", tabName = "tab1", icon = icon("dashboard")),
      menuItem("Tab 2", tabName = "tab2", icon = icon("rocket"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1
      tabItem(
        tabName = "tab1",
        fluidRow(
          # First Row - Value Boxes and Input Control
          valueBoxOutput("value_box1", width = 4),
          valueBoxOutput("value_box2", width = 4),
          selectInput("selected_var", "Select Variable", choices = colnames(mtcars)),
          actionButton("update_plot", "Update Graphs")
        ),
        fluidRow(
          # Second Row - Graphs
          box(
            title = "Scatter Plot", status = "info", solidHeader = TRUE, width = 4,
            plotlyOutput("scatter_plot", height = 400)
          ),
          box(
            title = "Time Series", status = "primary", solidHeader = TRUE, width = 4,
            dygraphOutput("time_series", height = 400)
          ),
          box(
            title = "Box Plot", status = "success", solidHeader = TRUE, width = 4,
            plotlyOutput("box_plot", height = 400)
          ),
          box(
            title = "Histogram", status = "warning", solidHeader = TRUE, width = 4,
            plotlyOutput("histogram", height = 400)
          ),
          box(
            title = "Line Chart", status = "danger", solidHeader = TRUE, width = 4,
            plotlyOutput("line_chart", height = 400)
          )
        )
      ),
      
      # Tab 2
      tabItem(
        tabName = "tab2",
        fluidRow(
          # First Row - Value Box and Input Control
          valueBoxOutput("value_box3", width = 4),
          selectInput("selected_var_tab2", "Select Variable", choices = colnames(mtcars)),
          actionButton("update_plot_tab2", "Update Graphs")
        ),
        fluidRow(
          # Second Row - Graphs
          box(
            title = "Bar Chart", status = "warning", solidHeader = TRUE, width = 4,
            plotlyOutput("bar_chart", height = 400)
          ),
          box(
            title = "Map", status = "danger", solidHeader = TRUE, width = 4,
            leafletOutput("map", height = 400)
          ),
          box(
            title = "Pie Chart", status = "info", solidHeader = TRUE, width = 4,
            plotlyOutput("pie_chart", height = 400)
          ),
          box(
            title = "Donut Chart", status = "primary", solidHeader = TRUE, width = 4,
            plotlyOutput("donut_chart", height = 400)
          ),
          box(
            title = "Bubble Chart", status = "success", solidHeader = TRUE, width = 4,
            plotlyOutput("bubble_chart", height = 400)
          )
        )
      )
    )
  ),
  skin = "blue"
)

# Define server logic
server <- function(input, output, session) {
  
  # Server code for Value Box 1
  output$value_box1 <- renderValueBox({
    valueBox(
      value = mean(mtcars$mpg),
      subtitle = "Average MPG",
      icon = icon("tachometer"),
      color = "purple"
    )
  })
  
  # Server code for Value Box 2
  output$value_box2 <- renderValueBox({
    valueBox(
      value = mean(mtcars$hp),
      subtitle = "Average Horsepower",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  # Server code for Value Box 3
  output$value_box3 <- renderValueBox({
    valueBox(
      value = mean(mtcars$qsec),
      subtitle = "Average Quarter Mile Time",
      icon = icon("stopwatch"),
      color = "blue"
    )
  })
  
  # Server code for Scatter Plot in Tab 1
  output$scatter_plot <- renderPlotly({
    plot_ly(data = mtcars, x = ~get(input$selected_var), y = ~hp, color = ~cyl, type = "scatter", mode = "markers") %>%
      layout(title = paste("Scatter Plot of", input$selected_var, "vs. Horsepower"),
             xaxis = list(title = input$selected_var),
             yaxis = list(title = "Horsepower"))
  })
  
  # Server code for Time Series Plot in Tab 1
  output$time_series <- renderDygraph({
    dygraph(mtcars[, c("mpg", "disp")], main = "MPG and Displacement Over Time") %>%
      dyOptions(colors = c("purple", "orange"))
  })
  
  # Ensure a default variable for Box Plot
  default_var <- "mpg"
  selected_var <- reactiveVal(default_var)
  
  # Server code for Box Plot in Tab 1 (triggered by action button)
  observeEvent(input$update_plot, {
    req(input$selected_var)  # Ensure a variable is selected
    selected_var(input$selected_var)
    
    output$box_plot <- renderPlotly({
      plot_ly(data = mtcars, x = ~cyl, y = ~get(selected_var()), type = "box") %>%
        layout(title = paste("Box Plot of", selected_var(), "by Cylinder Count"),
               xaxis = list(title = "Cylinder Count"),
               yaxis = list(title = selected_var()))
    })
  })
  
  # Server code for Histogram in Tab 1 (controlled by dropdown)
  output$histogram <- renderPlotly({
    plot_ly(data = mtcars, x = ~get(input$selected_var), type = "histogram") %>%
      layout(title = paste("Histogram of", input$selected_var),
             xaxis = list(title = input$selected_var),
             yaxis = list(title = "Frequency"))
  })
  
  # Server code for Line Chart in Tab 1
  output$line_chart <- renderPlotly({
    req(input$selected_var)  # Ensure a variable is selected
    plot_ly(data = mtcars, x = ~get(input$selected_var), y = ~disp, type = "line") %>%
      layout(title = paste("Line Chart:", input$selected_var, "vs. Displacement"),
             xaxis = list(title = input$selected_var),
             yaxis = list(title = "Displacement"))
  })
  
  # Server code for Bar Chart in Tab 2 (controlled by dropdown)
  output$bar_chart <- renderPlotly({
    plot_ly(data = mtcars, x = ~gear, y = ~get(input$selected_var_tab2), type = "bar", marker = list(color = ~hp)) %>%
      layout(title = paste("Bar Chart of", input$selected_var_tab2, "by Gear"),
             xaxis = list(title = "Gear"),
             yaxis = list(title = input$selected_var_tab2))
  })
  
  # Server code for Map in Tab 2
  output$map <- renderLeaflet({
    leaflet(data = mtcars) %>%
      addTiles() %>%
      addMarkers(lng = ~mpg, lat = ~disp, popup = ~paste("MPG:", mpg, "<br>Disp:", disp))
  })
  
  # Server code for Pie Chart in Tab 2
  output$pie_chart <- renderPlotly({
    plot_ly(data = mtcars, labels = ~gear, values = ~disp, type = "pie") %>%
      layout(title = "Pie Chart: Displacement by Gear Count", showlegend = FALSE) %>%
      add_trace(hole = 0.6)
  })
  
  # Server code for Donut Chart in Tab 2
  output$donut_chart <- renderPlotly({
    plot_ly(data = mtcars, labels = ~cyl, values = ~mpg, type = "pie") %>%
      layout(title = "Donut Chart: MPG by Cylinder Count", showlegend = FALSE) %>%
      add_trace(hole = 0.6)
  })
  
  # Server code for Bubble Chart in Tab 2 (controlled by slider)
  observeEvent(input$update_plot_tab2, {
    req(input$selected_var_tab2)  # Ensure a variable is selected
    output$bubble_chart <- renderPlotly({
      plot_ly(data = mtcars, x = ~gear, y = ~mpg, size = ~hp, color = ~cyl, type = "bubble") %>%
        layout(title = "Bubble Chart: MPG, Gear, Horsepower")
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
