#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("readr")
library("methods")
library("tools")
library("dplyr")
library("plyr")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ink Tank Net Profit Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("shopifyData",
                        "Upload CSV file taken from Shopify",
                        multiple=FALSE,
                        accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                      ),
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("netProfits")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$netProfits <- renderTable({
        #make sure file is uploaded
        req(input$shopifyData)
        
        #want "Net sales" column
        #need to sum up total from orders (read Order column OR Order.ID and track same number - will be continuous)
        
        #SPOD shipping costs so net profits can be calculated
        # 0.01-15.50 = 3.57
        # 15.51 - 27.50 = 4.55
        # 27.51 -55 = 7.80
        # 55.01 - 75 = 12
        # 75.01 - 100 = 17.50
        # 100 and above = 25
        mod_name <- strsplit(paste(input$shopifyData), " ")
        mod_name <- mod_name[[1]]
        df <- read.csv(mod_name,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        df <- df %>% select(Order.ID,Order,Shipping.region,Product.vendor,Product,Net.sales,Taxes,Total.sales)
        if (input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
