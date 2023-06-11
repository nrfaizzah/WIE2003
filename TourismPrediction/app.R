#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('rsconnect')
library(shiny)
library(shinythemes)
#install.packages("DT")
library(DT)
library(dplyr)
library(ggplot2)

data <- read.csv("C:/Users/faizz/OneDrive/Desktop/WIE2003/Cleandata.csv", header = TRUE)

# Define spending level categories
spending_levels <- c("Low", "Medium", "High")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme=shinytheme("sandstone"),#return URL of a shiny theme
  #themeSelector(),
  #theme = bs_theme(version = 4, bootswatch = "minty)",
  # Application title
  titlePanel("Tourism Marketing Segmentation Prediction"),
  navbarPage(
    title = "Tourism Marketing Segmentation Prediction",
    id = "nav",
    tabPanel("Home",value = "home",
             
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("type",
                             label = "Type of travelers: ",
                             c("Tourist " = "Tourist",
                               "Excursionist" = "Excursionist"
                             )),
                 selectInput("year",
                             label = "Year:",
                             c("2000" = "2000",
                               "2001" = "2001",
                               "2002" = "2002",
                               "2003" = "2003",
                               "2004" = "2004",
                               "2005" = "2005",
                               "2006" = "2006",
                               "2007" = "2007",
                               "2008" = "2008",
                               "2009" = "2009",
                               "2010" = "2010",
                               "2011" = "2011",
                               "2012" = "2012",
                               "2013" = "2013",
                               "2014" = "2014",
                               "2015" = "2015",
                               "2016" = "2016",
                               "2017" = "2017",
                               "2018" = "2018",
                               "2019" = "2019",
                               "2020" = "2020",
                               "2021" = "2021")),
                 #submitButton("Submit)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 h6(
                   p("Click to view data ",
                     a("excursionist expenditure", href = "https://data.gov.my/data/en_US/dataset/domestic-tourism-expenditure-of-excursionist-by-products/resource/4766b545-e49d-448a-b7b3-eead1c321684" )," and ",
                     a("tourist expenditure", href = "https://www.data.gov.my/data/ms_MY/dataset/inbound-tourism-expenditure-of-tourist-by-products/resource/826ae021-15f2-4840-a81c-7a63c19ce32a" ),".")
                 ),
                 h2("Tourism Expenditure"),
                 tabsetPanel(type = "tabs",
                             tabPanel("Explore", DT::dataTableOutput("table")),
                             tabPanel("Visualize Data",plotOutput("plot1")),
                             #tabPanel("Predictions"))
                 )))),
    
    tabPanel("Prediction", value = "Prediction",
             sidebarLayout(
               sidebarPanel(
                 selectInput("traveler", "Select Traveler Type",
                             choices = c("Excursionist", "Tourist"),
                             selected = "Excursionist"),
                 selectInput("product", "Select Product",
                             choices = unique(data$Product),
                             selected = NULL),
                 numericInput("years", "Number of Future Years",
                              value = 1, min = 1, max = 7),
                 actionButton("predictBtn", "Predict")
               ),
               
               #submitButton("Submit)
               
               # Show a plot of the generated distribution
               mainPanel(
                 h6(
                   p("Prediction for Expenditure of Tourist and Excursionist in future years (max= 7 years)."),
                   p("The result will display existing data from 2000-2021 and the selected number of future years for prediction.")),
                 h2("Expenditure Predictions "),
                 tabsetPanel(type = "tabs",
                             tabPanel("Explore", tableOutput("predictionTable")),
                             tabPanel("Visualize Data",plotOutput("predictionPlot")),
                             
                 ))  
             )),
    
    tabPanel("Comparision", value = "Comparision",
             sidebarLayout(
               sidebarPanel(
                 selectInput("products", "Select Product",
                             choices = unique(data$Product),
                             selected = NULL),
                 numericInput("yearspredict", "Number of Future Years",
                              value = 1, min = 1, max = 7),
                 actionButton("compareBtn", "Compare")
               ),
               
               #submitButton("Submit)
               
               # Show a plot of the generated distribution
               mainPanel(
                 h6(
                   p("Comparing the data for both type of travelers.")),
                 h2("Expenditure Comparision"),
                 tabsetPanel(type = "tabs",
                             tabPanel("Explore (Prediction)",tableOutput("predictionTableCompare")),
                             tabPanel("Visualize Data (Prediction)",plotOutput("predictionPlotCompare")),
                             
                 ))  
             )),
    
    tabPanel("Classification", value = "Classification",
    sidebarLayout(
      sidebarPanel(
        selectInput("productselected", "Select Product",
                    choices = unique(data$Product),
                    selected = NULL),
        selectInput("expenditure", "Select Expenditure",
                    choices = c("Excursionist", "Tourist"),
                    selected = "Excursionist")
      ),
      mainPanel(
        h2("Spending Level Classification"),
        tableOutput("spendingTable")
      )
    )
  ),
    
    tabPanel("Guideline", fluid = TRUE,
             fluidRow(
               column(6,
                      #br(),
                      h1(p("User Manual")),
                      tags$p(HTML("1. App start with default choices for their preferences. The list preferences and the choices are: ")),
                      tags$ul(
                        tags$li(HTML("Type of tourism : Excursionist or Tourist")),
                        tags$li(HTML("Year : Year from 2000 to 2021"))
                      ),
                      tags$p(HTML("2. User will choose their preference in tourism type to see the prediction.")),
                      tags$p(HTML("3. The total expenditure product based on year for each preference can be found at link provided (excursionist expenditure) and (tourist expenditure) .")),
                      tags$p(HTML("4. The app wil automatically list out all ranking for product spent by choosen preferences.Prediction for both type of travelers and classification based on the spending value.")),
                      tags$p(HTML("5. It will also automatically disploy plot graph to visualize the data.")),
                      tags$p(HTML("6. User can now comes up with solution for marketing tourism segmentation for future tourism in Malaysia."))
                      #hr(),
               )
               
             )),
    tabPanel("About",fluid = TRUE ,
             fluidRow(
               column(6,
                      #br(),
                      h1(p("About the project")),
                      tags$p(HTML("Our system focuses on analyzing and distinguishing between excursionists and tourists visiting Malaysia, comparing their numbers accross different years, and raking their significance within Malaysia's tourism industry.")),
                      tags$p(HTML("Our aim is to provide valuable insights and support decision-making processes for stakeholders in tourism sector.")),
                      tags$p(HTML("Problem faced:")),
                      tags$ul(
                        tags$li(HTML("Understanding the composition and impact of the visitors to Malaysia is crucial for effective tourism planning and development.Howeve, distinguishing between excursionists and tourists, analyzing their trends over time, and ranking their contributions can be challenging without the right tools and methodologies.")),
                        
                      ),
                      tags$p(HTML("Our Solution:")),
                      tags$p(HTML("To address these challenges, we developed a comprehensive system that enables the identification, comparison, and ranking of excursionists and tourists within Malaysia's tourism landscape.")),
                      tags$p(HTML("Here's how our project works:")),
                      tags$ul(
                        tags$li(HTML("Visualizing Comparative Statistics: Our system offers intuitive visualizations of key statistics related to excursionists and tourists across different years and Malaysian destinations. This helps users to compare and assess the impact and trends of these visitor segments, enabling informed decision-making and strategic planning.")),
                        tags$li(HTML("Providing Comprehensive Rankings: Our system generates rankings that reflect the relative importance of different years within Malaysia's tourism industry. By considering various factors such as visitor numbers, duration of stay, and overall contribution, our rankings provide a comprehensive understanding of each year's significance, assisting stakeholders in identifying key periods for tourism planning and investment.")),
                        tags$li(HTML("Supporting Decision-Making: Our project aims to support decision-making processes in the tourism industry. By providing valuable insights, comparative analysis, and rankings, our system equips stakeholders, such as tourism boards, travel agencies, and government bodies, with the information they need to make data-driven decisions, shape policies, and implement targeted strategies")),
                      ),
                      h1(p("Our hopes")),
                      tags$p(HTML("Through our data-driven approach, we enable stakeholders in the tourism industry to gain a deeper understanding of the visitor composition, trends, and economic impact associated with excursionists and tourists. By providing comprehensive visualizations, rankings, and insights, our project empowers stakeholders to make informed decisions that enhance the tourism experience and contribute to the sustainable growth of Malaysia's tourism sector.")),
                      h1(p("Data Sources")),
                      tags$ul(
                        tags$li(HTML("<a href=\"https://data.gov.my/data/en_US/dataset/domestic-tourism-expenditure-of-excursionist-by-products/resource/4766b545-e49d-448a-b7b3-eead1c321684\">https://data.gov.my/data/en_US/dataset/domestic-tourism-expenditure-of-excursionist-by-products/resource/4766b545-e49d-448a-b7b3-eead1c321684</a>.")),
                        tags$li(HTML("<a href=\"https://www.data.gov.my/data/ms_MY/dataset/inbound-tourism-expenditure-of-tourist-by-products/resource/826ae021-15f2-4840-a81c-7a63c19ce32a\">https://www.data.gov.my/data/ms_MY/dataset/inbound-tourism-expenditure-of-tourist-by-products/resource/826ae021-15f2-4840-a81c-7a63c19ce32a</a>.")),
                        
                      )
               )),
             
    ),
    
    tabPanel("Contributors", fluid = TRUE,
             fluidRow(
               column(6,
                      #br(),
                      h2(p("Group 2")),
                      tags$p(HTML("Members: ")),
                      tags$ul(
                        tags$li(HTML("Trishan Raj A/L Shanmugam (22004733/1)")),
                        tags$li(HTML("Siti Nurfaizzah Binti Rahami (U2001314/2)")),
                        tags$li(HTML("Hananiah Basyirah Binti Mohd Bakri (U2001271/2)")),
                        tags$li(HTML("Oh Qi Hen (S2151876/1)"))
                      )
               )
             )
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #Home(Explore)
  output$table <- renderDataTable({
    specific_year <- input$year
    specific_travelers <- input$traveler
    
    filtered_data <- subset(data, Year == specific_year)
    selected_data <- filtered_data[,c("Product","Year",specific_travelers),drop = FALSE]
    modified_data <- selected_data
    
    #change column name 
    if("Excursionist" %in% colnames(modified_data)){
      colnames(modified_data)[colnames(modified_data) == "Excursionist" ] <- "Expenditure Value (RM Millions)"
    }
    
    if("Tourist" %in% colnames(modified_data)){
      colnames(modified_data)[colnames(modified_data) == "Tourist" ] <- "Expenditure Value (RM Millions)"
    }
    
    modified_data
  })
  
  #Home(Visualize)
  output$plot1 <- renderPlot({
    specific_year <- input$year
    specific_travelers <- input$type
    
    filtered_data <- subset(data, Year == specific_year)
    if (specific_travelers == "Excursionist") {
      plot_data <- filtered_data$Excursionist
      plot_title <- "Excursionist Expenditure based on Product"
    } else {
      plot_data <- filtered_data$Tourist
      plot_title <- "Tourist Expenditure based on Product"
    }
    
    plot_colors <- "steelblue"  # Define the color for the plot
    x_labels <- filtered_data$Product  # Use the 'Product' column as x-axis labels
    
    # Filter out missing or non-numeric values in plot_data
    plot_data <- plot_data[is.finite(plot_data)]
    
    # Create a more interesting plot
    if (length(plot_data) > 0) {
      x <- seq_along(plot_data)
      
      # Set valid y-axis limits
      y_limits <- c(0, max(plot_data) * 1.2)
      if (!all(is.finite(y_limits))) {
        y_limits <- c(0, 1)  # Fallback to default limits if non-finite values exist
      }
      
      plot(x, plot_data, type = "o", pch = 19, col = plot_colors,
           xlim = c(1, length(plot_data)), ylim = y_limits,
           xlab = "Product", ylab = "Expenditure (RM Millions)", main = plot_title)
      
      # Add gridlines to the plot
      grid(lty = "dotted")
      
      # Add labels to each data point
      text(x, plot_data, labels = plot_data, pos = 3, cex = 0.8)
      
      # Customize x-axis ticks and labels
      axis(1, at = x, labels = x_labels, las = 2, cex.axis = 0.8)
    } else {
      # Handle case when plot_data is empty
      plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "",
           main = plot_title)
      text(0.5, 0.5, "No data available", col = "red", cex = 1.2)
    }
  })
  
  #Prediction
  predictions <- eventReactive(input$predictBtn, {
    specific_traveler <- input$traveler
    specific_product <- input$product
    future_years <- input$years
    
    filtered_data <- data %>%
      filter(Product == specific_product)
    
    max_year <- max(filtered_data$Year)
    max_expenditure <- filtered_data[[specific_traveler]][filtered_data$Year == max_year]
    
    # Perform regression on the available data
    regression_model <- lm(formula = paste(specific_traveler, "~ Year"), data = filtered_data)
    
    # Create a data frame for future predictions
    future_data <- data.frame(
      Year = seq(max_year + 1, max_year + future_years),
      stringsAsFactors = FALSE
    )
    
    # Predict the expenditure for future years using the regression model
    future_data[[specific_traveler]] <- predict(regression_model, newdata = future_data)
    future_data$Product <- rep(specific_product, future_years)
    
    # Combine the available data and predicted data
    prediction_data <- bind_rows(filtered_data, future_data)
    
    prediction_data
  })
  
  #Prediction(Explore)
  output$predictionTable <- renderTable({
    predicted_data <- predictions()
    
    specific_traveler <- input$traveler
    
    if (!is.null(predicted_data)) {
      predicted_data <- predicted_data %>%
        select(Year, Expenditure = .data[[specific_traveler]])
    }
    
    colnames(predicted_data)[2] <- "Expenditure (RM Millions)"
    
    predicted_data
  })
  
  #Prediction(Visualize)
  output$predictionPlot <- renderPlot({
    prediction_data <- predictions()
    
    specific_traveler <- input$traveler
    specific_product <- input$product
    
    if (!is.null(prediction_data)) {
      ggplot(prediction_data, aes(x = Year, y = .data[[specific_traveler]], color = Product)) +
        geom_point(size = 3) +
        geom_line() +
        geom_text(aes(label = round(.data[[specific_traveler]], 2)), hjust = -0.2, size = 3, color = "black") +
        labs(x = "Year", y = "Expenditure (RM Millions)", title = paste(specific_traveler, "Expenditure for", specific_product)) +
        theme_minimal()
    }
  })

  #compare prediction
  comparepredictions <- eventReactive(input$compareBtn, {
    specific_products <- input$products
    future_yearspredict <- input$yearspredict
    
    filtered_data <- data %>%
      filter(Product == specific_products)
    
    max_year <- max(filtered_data$Year)
    
    # Perform regression on the available data for both traveler types
    excursionist_model <- lm(formula = "Excursionist ~ Year", data = filtered_data)
    tourist_model <- lm(formula = "Tourist ~ Year", data = filtered_data)
    
    # Create a data frame for future predictions
    future_data <- data.frame(
      Year = seq(max_year + 1, max_year + future_yearspredict),
      stringsAsFactors = FALSE
    )
    
    # Predict the expenditure for future years using the regression models
    future_data$Excursionist <- predict(excursionist_model, newdata = future_data)
    future_data$Tourist <- predict(tourist_model, newdata = future_data)
    future_data$Product <- rep(specific_products, future_yearspredict)
    
    # Combine the available data and predicted data
    compareprediction_data <- bind_rows(filtered_data, future_data)
    
    compareprediction_data
  })
  
  output$predictionTableCompare <- renderTable({
    comparepredicted_data <- comparepredictions()
    
    if (!is.null(comparepredicted_data)) {
      comparepredicted_data <- comparepredicted_data %>%
        select(Year, Excursionist, Tourist)
      colnames(comparepredicted_data)[2] <- "Excursionist Expenditure (RM Millions)"
      colnames(comparepredicted_data)[3] <- "Tourist Expenditure (RM Millions)"
    }
    
    comparepredicted_data
  })
  
  output$predictionPlotCompare <- renderPlot({
    compareprediction_data <- comparepredictions()
    
    specific_products <- input$products
    
    if (!is.null(compareprediction_data)) {
      ggplot(compareprediction_data, aes(x = Year)) +
        geom_point(aes(y = Excursionist, color = "Excursionist"), size = 3) +
        geom_point(aes(y = Tourist, color = "Tourist"), size = 3) +
        geom_line(aes(y = Excursionist, color = "Excursionist")) +
        geom_line(aes(y = Tourist, color = "Tourist")) +
        geom_text(aes(x = Year, y = Excursionist, label = round(Excursionist, 2)), hjust = -0.2, size = 3, color = "black") +
        geom_text(aes(x = Year, y = Tourist, label = round(Tourist, 2)), hjust = -0.2, size = 3, color = "black") +
        labs(x = "Year", y = "Expenditure (RM Millions)", title = paste("Expenditure for", specific_products)) +
        scale_color_manual(values = c("Excursionist" = "blue", "Tourist" = "red")) +
        theme_minimal()
    }
  })

  # Perform spending level classification
  classified_data <- reactive({
    specific_productselected <- input$productselected
    specific_expenditure <- input$expenditure
    
    filtered_data <- data %>%
      filter(Product == specific_productselected)
    
    spending_levels <- cut(filtered_data[[specific_expenditure]],
                           breaks = c(-Inf, 1000, 4500, Inf),
                           labels = spending_levels,
                           include.lowest = TRUE)
    
    classified_data <- data.frame(
      Year = filtered_data$Year,
      Expenditure = filtered_data[[specific_expenditure]],
      SpendingLevel = spending_levels
    )
    
    classified_data
  })
  
  # Display the classified data in a table
  output$spendingTable <- renderTable({
    classified_data()
  })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
