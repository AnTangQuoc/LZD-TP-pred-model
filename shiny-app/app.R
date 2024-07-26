library(tidyverse)
library(shiny)
library(bslib)
library(BAS)

# file is tied to dev branch, will later need a separate prod branch
load("model-black-box.rda")

# Define UI for app ----
ui <- page_sidebar(
  # App title ----
  title = "Demonstration App",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    checkboxGroupInput(
      inputId = "binary_vars",
      label = "Characteristics",
      choices = c(
        "LZD duration ≥ 14 days",
        "Heparin use",
        "Enoxaparin use",
        "Sepsis",
        "Cirrhosis",
        "CRRT",
        "ICU admission"
      )
    ),
    numericInput(
      inputId = "baseline_CLCR",
      label = "Baseline Creatinine Clearance (mL/min)",
      value = 90,
      min = 0,
      max = 400
    ),
    numericInput(
      inputId = "baseline_PLT",
      label = "Baseline Platelet Count (G/L)",
      value = 150,
      min = 0,
      max = 450
    ),
    numericInput(
      inputId = "patient_age",
      label = "Patient Age (years)",
      value = 20,
      min = 18,
      max = 105
    )
  ),
  value_box(
    title = "LAT risk of patient is",
    value = textOutput(outputId = "prediction")
  )
)

# Define server logic----
server <- function(input, output, session) {
  output$prediction <- renderText({
    new_data <- tibble(
      LZD_duration_14 = c("LZD duration ≥ 14 days" %in% input$binary_vars),
      comed_heparin = c("Heparin use" %in% input$binary_vars),
      comed_enoxaparin = c("Enoxaparin use" %in% input$binary_vars),
      infect_sepsis = c("Sepsis" %in% input$binary_vars),
      comorb_cirr = c("Cirrhosis" %in% input$binary_vars),
      invasive_CRRT = c("CRRT" %in% input$binary_vars),
      dept_ICU_ER = c("ICU admission" %in% input$binary_vars),
      baseline_CLCR = input$baseline_CLCR,
      baseline_PLT = input$baseline_PLT,
      patient_age = input$patient_age
    )
    prediction <- predict(
      model_black_box,
      newdata = new_data, type = "response"
    ) |> _$fit
    paste0(signif(prediction * 100, 3), "%")
  })
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)

# run following code to start app
# note that the app is currently local only
# shiny::runApp("shiny-app/app.R")