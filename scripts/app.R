### load packages ----
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)

### load Yolo Bypass monitoring data from EDI ----
delta_file <- "https://pasta.lternet.edu/package/data/eml/edi/233/2/015e494911cf35c90089ced5a3127334" 

delta_data <- read.csv(delta_file) |>
  mutate(SampleDate = mdy(SampleDate)) |>
  filter(grepl("Salmon|Striped Bass|Smelt|Sturgeon", CommonName))

species_choices <- sort(unique(delta_data$CommonName))

### define UI ----
ui <- page_sidebar(
  title = "Yolo Bypass Fish Monitoring",
  sidebar = sidebar(

    ### date range slider ----
    sliderInput(
      inputId = "date_range",
      label = "Select date range:",
      min = as.Date("1998-01-01"),
      max = as.Date("2018-12-31"),
      value = c(as.Date("1998-01-01"), as.Date("2018-12-31"))
    ),

    ### species checkbox====
    checkboxGroupInput(
      inputId = "species_select",
      label = "Select species",
      choices = species_choices,
      selected = species_choices
    )

  ), ### END sidebar

  ### Secchi depth plot ----
  card(
    plotOutput(outputId = "secchi_plot")
  ),
  ### fish catch bar chart ----
  card(
    plotOutput(outputId = "catch_plot")
  )

) ### END page_sidebar

### define server ----
server <- function(input, output) {

  ### filter data by date range ----
  filtered_data <- reactive({
    delta_data |>
      filter(
        SampleDate >= input$date_range[1],
        SampleDate <= input$date_range[2],
        CommonName %in% input$species_select
      )
  })

  ### render Secchi depth plot ----
  output$secchi_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = SampleDate, y = Secchi)) +
      geom_point(color = "steelblue", alpha = 0.5) +
      labs(x = "Date", y = "Secchi depth (m)") +
      theme_light()
  })

  ### render fish catch bar chart ----
  output$catch_plot <- renderPlot({
    filtered_data() |>
      group_by(CommonName) |>
      summarize(total_catch = sum(Count, na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(x = reorder(CommonName, -total_catch),
                 y = total_catch,
                 fill = CommonName)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Species", y = "Total catch") +
      theme_light()
  })

}

### launch the app ----
shinyApp(ui = ui, server = server)
