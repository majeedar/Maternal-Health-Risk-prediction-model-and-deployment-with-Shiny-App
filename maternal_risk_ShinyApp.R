library(shiny)
library(randomForest)

# Load the pre-trained Random Forest model
model <- readRDS("model.rds")

# Define UI for the application
ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Maternal Health Risk Predictor'),
  
  # Input values
  sidebarPanel(
    tags$label(h3('Input parameters')),
    numericInput("Age", 
                 label = "Age", 
                 value = 25),
    numericInput("SystolicBP", 
                 label = "Systolic Blood Pressure", 
                 value = 120),
    numericInput("DiastolicBP", 
                 label = "Diastolic Blood Pressure", 
                 value = 80),
    numericInput("BS", 
                 label = "Blood Sugar", 
                 value = 5),
    numericInput("BodyTemp", 
                 label = "Body Temperature", 
                 value = 98.6),
    numericInput("HeartRate", 
                 label = "Heart Rate", 
                 value = 70),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
  )
)

# Define server logic required to make predictions
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # Create a data frame with input values
    df <- data.frame(
      Age = input$Age,
      SystolicBP = input$SystolicBP,
      DiastolicBP = input$DiastolicBP,
      BS = input$BS,
      BodyTemp = input$BodyTemp,
      HeartRate = input$HeartRate
    )
    
    # Make prediction using the loaded model
    prediction <- predict(model, df)
    
    # Prepare output
    Output <- data.frame(
      Prediction = prediction
    )
    
    return(Output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(datasetInput()) 
    } 
  })
}

# Create the shiny app
shinyApp(ui = ui, server = server)
