library(shiny)


# Define UI for Total payroll by LA City
ui <- fluidPage(
  
  # App title ----
  titlePanel("Total payroll by LA  City"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "paytype",
                  label = "Choose a type of payroll:",
                  choices = c("Total payments", "Base pay", "Overtime pay", 
                              "Other pay")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "tophp",
                   label = "Year:",
                   value = 2017),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Top n highest paid LA employees:",
                   value = 10),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "topearn",
                   label = "Top n earning departments:",
                   value = 5),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "topexp",
                   label = "Top n expensive departments:",
                   value = 5)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
    # Output: Visualiza the total LA City payroll for each year
    plotOutput(outputId = "totpay"),
      
    # Output: Vasualize the payroll information of the top n highest paid LA City employees in a specific year
    tableOutput("higestp"),
      
    # Output: Vasualize the mean or median payroll of top n earning departments
    tableOutput("higestd"),
      
    # Output: Vasualize the total payroll of top n expensive departments
    tableOutput("expd")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset of payroll type ----
  PaytypeInput <- reactive({
    switch(input$paytype,
           "Total payments" = Total_Payments,
           "Base pay" = LACEP$Base_Pay,
           "Overtime pay" = LACEP$Overtime_Pay,
           "Other pay" = LACEP$Other_Pay_Payroll_Explorer)
  })
  
  
  
  # Visualiza the total LA City payroll for each year----
  output$distplot <- renderPlot({
    x <- datasetInput()
    bins <- seq(min(x), max(x))
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Payments",
         main = "Total payroll by LA City")
    
  })
  
  # Show the payroll information of the top n highest paid special n and year  ----
  output$highestp <- renderTable({
    tibble(
      PayrollType = head(datasetInput(), n = input$obs)
      
    )
  })
    
}

shinyApp(ui, server)