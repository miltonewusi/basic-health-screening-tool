library(shiny)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
  
  theme = shinytheme("flatly"),  # Modern clean theme
  
  titlePanel(div(icon("heartbeat", class = "text-danger"), " Basic Health Screening Tool")),
  
  sidebarLayout(
    
    sidebarPanel(
      h4(icon("user", class = "text-primary"), " Client Information"),
      
      numericInput("age", label = tagList(icon("calendar"), "Age (years)"), value = 30, min = 1),
      selectInput("sex", label = tagList(icon("venus-mars"), "Sex"), choices = c("Male", "Female")),
      
      hr(),
      
      h4(icon("weight", class = "text-success"), " Anthropometry"),
      numericInput("weight", label = tagList(icon("balance-scale"), "Weight (kg)"), value = 70),
      numericInput("height", label = tagList(icon("arrows-alt-v"), "Height (cm)"), value = 170),
      
      hr(),
      
      h4(icon("stethoscope", class = "text-danger"), " Blood Pressure"),
      numericInput("sys", label = tagList(icon("arrow-up"), "Systolic (mmHg)"), value = 120),
      numericInput("dia", label = tagList(icon("arrow-down"), "Diastolic (mmHg)"), value = 80),
      
      br(),
      actionBttn("screen", "Run Screening", style = "fill", color = "primary", icon = icon("play"))
    ),
    
    mainPanel(
      h3(icon("clipboard-check", class = "text-info"), " Screening Results"),
      
      wellPanel(
        tags$strong("BMI Result:"), textOutput("bmi_out"),
        tags$strong("Blood Pressure Result:"), textOutput("bp_out")
      ),
      
      hr(),
      
      h3(icon("exclamation-triangle", class = "text-warning"), " Overall Risk Assessment"),
      wellPanel(
        tags$strong("Risk Level:"), textOutput("overall_risk"),
        tags$strong("Recommended Action:"), textOutput("recommendation")
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$screen, {
    
    ## BMI
    height_m <- input$height / 100
    bmi <- input$weight / (height_m^2)
    
    bmi_risk <- if (bmi < 18.5) {
      "Moderate Risk (Underweight)"
    } else if (bmi < 25) {
      "Low Risk (Normal)"
    } else if (bmi < 30) {
      "Moderate Risk (Overweight)"
    } else {
      "High Risk (Obese)"
    }
    
    ## BP
    bp_cat <- if (input$sys < 120 & input$dia < 80) {
      "Normal"
    } else if (input$sys < 130 & input$dia < 80) {
      "Elevated"
    } else if (input$sys < 140 | input$dia < 90) {
      "Stage 1 Hypertension"
    } else {
      "Stage 2 Hypertension"
    }
    
    bp_risk <- if (bp_cat == "Normal") {
      "Low Risk"
    } else if (bp_cat == "Elevated") {
      "Moderate Risk"
    } else if (bp_cat == "Stage 1 Hypertension") {
      "High Risk"
    } else {
      "Very High Risk"
    }
    
    ## Overall risk
    risks <- c(bmi_risk, bp_risk)
    
    overall <- if (any(grepl("Very High|High", risks))) {
      "HIGH RISK"
    } else if (any(grepl("Moderate", risks))) {
      "MODERATE RISK"
    } else {
      "LOW RISK"
    }
    
    recommendation <- switch(
      overall,
      "LOW RISK" = "✅ Encourage healthy lifestyle and routine check-ups.",
      "MODERATE RISK" = "⚠️ Provide lifestyle counselling and monitor regularly.",
      "HIGH RISK" = "🚨 Refer to a health facility for further assessment."
    )
    
    ## Outputs
    output$bmi_out <- renderText({
      paste0("BMI: ", round(bmi, 1), " → ", bmi_risk)
    })
    
    output$bp_out <- renderText({
      paste0("Blood Pressure: ", bp_cat, " → ", bp_risk)
    })
    
    output$overall_risk <- renderText({
      paste(overall)
    })
    
    output$recommendation <- renderText({
      recommendation
    })
    
  })
}

shinyApp(ui = ui, server = server)
