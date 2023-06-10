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

data <- read.csv("C:/Users/faizz/OneDrive/Desktop/WIE2003/Cleandata.csv", header = TRUE)
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
              a("tourist expenditure", href = "https://www.data.gov.my/data/ms_MY/dataset/inbound-tourism-expenditure-of-tourist-by-products/resource/826ae021-15f2-4840-a81c-7a63c19ce32a" ))
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
                 selectInput("type",
                             label = "Type of travelers: ",
                             c("Tourist " = "Tourist",
                               "Excursionist" = "Excursionist"
                             )),
                 selectInput("productInput", "Select Product:", choices = unique(data$Product)),
               
                numericInput("futureYearsInput", "Number of Future Years:", value = 5)
               ),
                
                 #submitButton("Submit)
               
               # Show a plot of the generated distribution
               mainPanel(
                 h6(
                   p("Prediction for Expenditure of Tourist and Excursionist in future years.")
                 ),
                 h2("Expenditure Predictions "),
                 tabsetPanel(type = "tabs",
                           tabPanel("Explore", verbatimTextOutput("predictionsOutput")),
                            tabPanel("Visualize Data",plotOutput("plot2")),
                          
                 ))  
             )),
    
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
                      tags$p(HTML("4. The app wil automatically list out all ranking for product spent by choosen preferences.")),
                      tags$p(HTML("5. It will also automatically disploy histogram to visualize the data.")),
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
                      h1(p("What distinguishes us from others ?")),
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

  output$table <- renderDataTable({
    specific_year <- input$year
    specific_travelers <- input$type
    
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
    
    output$plot1 <- renderPlot({
    
      specific_year <- input$year
      specific_travelers <- input$type
      
      filtered_data <- subset(data, Year == specific_year)
      if(specific_travelers == "Excursionist"){
        plot_data <- filtered_data$Excursionist
        plot_title <- "Excursionist Expenditure based on Product"
      }else{
       plot_data <- filtered_data$Tourist
       plot_title <- "Tourist Expenditure based on Product"
      }
    
      plot(plot_data, main = plot_title)
    })
    
    observeEvent(input$productInput, {
      selected_product <- input$productInput
      selected_expenditure_type <- input$type
      future_years <- input$futureYearsInput
      
      regression_data <- subset(data, Product == selected_product)
      x_reg <- regression_data$Year
      y_reg <- if (selected_expenditure_type == "Excursionist") {
        regression_data$Excursionist
      } else if (selected_expenditure_type == "Tourist") {
        regression_data$Tourist
      }
      
      reg_model <- lm(y_reg ~ x_reg, data = regression_data)
      future_years_seq <- seq(max(x_reg) + 1, max(x_reg) + future_years)
      predicted_values_reg <- predict(reg_model, newdata = data.frame(x_reg = future_years_seq))
      
      output$predictionsOutput <- renderPrint({
        result <- data.frame(Year = future_years_seq, Prediction = predicted_values_reg)
        result <- result[complete.cases(result), ]
        result
      })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
