library(shiny)
library(tidyverse)
library(ggplot2)

options(shiny.sanitize.errors= FALSE)
LACEP <- read_rds("LACEPdat.rds")

# Who earned most? Visualize the payroll information of the top n highest paid LA City employees in a specific year
ui <- fluidPage(
  
  # App title ----
  titlePanel("LA City Employee Payroll"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Selector for choosing year ----
      selectInput(inputId = "year",
                  label = "Choose a year of payroll:",
                  choices = c(2013, 2014, 2015, 2016, 2017, 2018),
                  selected = 2017),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Top n highest paid LA employees:",
                   value = 10),
    
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "topearn",
                 label = "Top n earning departments:",
                 value = 5),
    
      # Input: Selector for choosing method ----
      selectInput(inputId = "method",
                label = "Choose a method:",
                choices = c("mean", "median"),
                selected = "median" ),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "topexp",
                 label = "Top n expensive departments:",
                 value = 5)
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
    tabsetPanel(  
    # Output: Visualize the total LA City payroll of each year
    tabPanel("Total LA City payroll of each year", 
               plotOutput("TLAPY")),
      
    tabPanel("Top n highest paid LA City employees", 
             tableOutput("toppaide")),
    
    # Output: Vasualize the mean or median payroll of top n earning departments
    tabPanel("Mean or Median payroll of top n earning departments", 
             tableOutput("toppaidd")),
    
    # Output: Vasualize the total payroll of top n expensive departments
    tabPanel("Top n expensive departments", 
             tableOutput("topexpd"))
    )
   )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Show the total LA City payroll of each year, with breakdown into base pay, overtime pay, and other pay.
  output$TLAPY <- renderPlot({
    LACEPS <- LACEP %>%
      group_by(Year) %>%
      summarize(sumbp = sum(Base_Pay, na.rm = TRUE), 
                sumoverp <- sum(Overtime_Pay, na.rm = TRUE),
                sumbotherp <- sum(Other_Pay_Payroll_Explorer, na.rm = TRUE))
    names(LACEPS) <- c("Year", "Base_Pay", "Overtime_Pay", "Other_pay")
    LACEPS <- LACEPS %>% gather(Paytype, Payroll, -Year) %>%
      mutate(pos =  0.6* Payroll)
    s <- ggplot(LACEPS, aes(x = Year, y = Payroll, fill = Paytype)) 
    s + geom_bar(stat = "identity") +  geom_text(aes(label = Payroll), 
        position = position_stack(vjust = 0.5), size = 3) + 
      labs(x="Year", y="Payroll", title="Total payroll by LA City") +
      scale_x_discrete(name ="Year", limits=c(2013, 2014, 2015, 
                                              2016, 2017, 2018))
  })
    
    
  # Show the payroll information of the top n highest paid special n and year  ----
  output$toppaide <- renderTable({
    LACEPP <- LACEP %>%
      filter(Year == input$year) %>%
      select(Row_ID, Department_Title, Job_Class_Title, Total_Payments, 
             Base_Pay, Overtime_Pay, Other_Pay_Payroll_Explorer)
    
      head(arrange(LACEPP, desc(Total_Payments)), n = input$obs)
  })
  
  # Show the payroll information of the the mean or median payroll of top n earning departments  ----
  output$toppaidd <- renderTable({
    if (input$method == "mean"){
      LACEPDE <- LACEP %>%
       filter(Year == input$year) %>%
       select(Department_Title, Total_Payments, 
             Base_Pay, Overtime_Pay, Other_Pay_Payroll_Explorer) %>% 
       group_by(Department_Title) %>%
       summarise(Mean_Total_Payments = mean(Total_Payments, na.rm = T),
                Mean_Base_Pay = mean(Base_Pay, na.rm = T),
                Mean_Overtime_Pay = mean(Overtime_Pay, na.rm = T),
                Mean_Other_Pay = mean(Other_Pay_Payroll_Explorer, na.rm = T))
      
      head(arrange(LACEPDE, desc(Mean_Total_Payments)), n = input$topearn)
      }
    else if (input$method == "median"){
    LACEPDE <- LACEP %>%
      filter(Year == input$year) %>%
      select(Department_Title, Total_Payments, 
                 Base_Pay, Overtime_Pay, Other_Pay_Payroll_Explorer) %>% 
      group_by(Department_Title) %>%
      summarise(Median_Total_Payments = median(Total_Payments, na.rm = T),
            Median_Base_Pay = median(Base_Pay, na.rm = T),
            Median_Overtime_Pay = median(Overtime_Pay, na.rm = T),
            Median_Other_Pay = median(Other_Pay_Payroll_Explorer, na.rm = T))
    
      head(arrange(LACEPDE, desc(Median_Total_Payments)), n = input$topearn)
      }
   
  })
  
  # Show the payroll information of the total payroll of top n expensive departments  ----
  output$topexpd <- renderTable({
    LACEPDC <- LACEP %>%
      filter(Year == input$year) %>%
      select(Department_Title, Total_Payments, Base_Pay, Overtime_Pay,
             Other_Pay_Payroll_Explorer, Average_Benefit_Cost) %>%
       group_by(Department_Title) %>%
       summarise(Total_Cost = sum(Average_Benefit_Cost, na.rm = T), 
                Total_payments = sum(Total_Payments, na.rm = T),
                Base_Pay = sum(Base_Pay, na.rm = T),
                Overtime_Pay = sum(Overtime_Pay, na.rm = T),
                Other_Pay = sum(Other_Pay_Payroll_Explorer, na.rm = T))  

    head(arrange(LACEPDC, desc(Total_Cost)), n = input$topexp)
  })
  
}
    

shinyApp(ui, server)
