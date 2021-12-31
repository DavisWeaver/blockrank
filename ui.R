#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "footer-basic-centered.css")
    ),
    withMathJax(),
    # Application title
    
    fluidRow(
      column(
        width = 4,
        style='border-bottom: 1px solid black; height:60px',
        tags$h3(tags$a(href = "https://www.commiecoinasa.com/blockrank/support", "Support us"))
      ),
      column( 
        width = 4, 
        style='border-bottom: 1px solid black; height:60px',
        tags$h1(strong("BlockRank"), align = "center")
      ),
      column(
        width = 4, 
        style='border-bottom: 1px solid black; height:60px',
        tags$h3("Presented by", 
                tags$a(href="https://www.commiecoinasa.com", "Commie Coin"), 
                aligh = "right")
      )
    ),
    
    fluidRow(
      column(width = 3, style='border-right: 1px solid black',
             fluidRow(
               column(width = 10, offset = 1,
                      selectInput("asa_id", "ASA ID:", 
                                  c("Commie Coin (USSR)",
                                    "BirdBot (BIRDS)",
                                    "AlgoMeow (MEOW)", 
                                    "Akita Inu (AKITA)",
                                    "Svansy Coin (SVANSY)", "MoonX (MOONX)", 
                                    "Matrix (MTRX)", 
                                    "CryptoRulesEverythingAroundMe (CREAM)"))
               )
             ),
             fluidRow(
               column(width = 10, offset = 1,
                      # Display a table with the nodes data
                      tags$h4("Number of Wallets:"),
                      textOutput("wallet_number"),
                      tags$h4("Blacklisted Wallets:"),
                      textOutput("blacklist_number"),
                      tags$h4("Suspicious Wallets:"),
                      textOutput("sus_number"),
                      tags$br(),
                      actionButton("add_sus", "Add Suspicious Wallets to Blacklist"),
                      tags$br(),
                      tags$br(),
                      downloadButton("download", "Download Current Blacklist")
               )
             )
      ),
      # which node did we delete? just to remind people 
      column(width = 9, 
             
             fluidRow(
               #draw the visnetwork
               visNetworkOutput("main_network", height = "600px")
             )
             
      )
      
    ), 
    fluidRow(
      wellPanel(

        includeHTML("./www/include_footer.html")
  
      ),
      
    )
    
    
  )
)

