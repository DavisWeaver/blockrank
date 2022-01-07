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
        tags$h3(tags$a(href = "mailto:davis@block-rank.io", "Contact us"))
      ),
      column( 
        width = 4, 
        style='border-bottom: 1px solid black; height:60px',
        tags$h1(strong("BlockRank"), align = "center")
      ),
      column(
        width = 4, 
        style='border-bottom: 1px solid black; height:60px',
        tags$img(src = "birdbot.jpeg", width = "40px", height = "55px",
                 align = "right")
      )
    ),
    
    fluidRow(
      column(width = 3, style='border-right: 1px solid black',
             fluidRow(
               column(width = 10, offset = 1,
                      selectInput("asa_id", "ASA ID:", 
                                  c("BirdBot (BIRDS)"))
               )
             ),
             fluidRow(
               column(width = 10, offset = 1,
                      # mess around with different network filtering stuff
                      numericInput("min_holding", "Minimum ASA Holding", value = 200000, min = 0),
                      tags$br(),
                      actionButton("add_sus", "Add Suspicious Wallets to Blacklist"),
                      tags$br(),
                      tags$br(),
                      actionButton("update_networks", "Fetch most recent transactions"),
                      tags$br(),
                      tags$br(),
                      downloadButton("download", "Download Current Blacklist")
               )
             )
      ),
      # 
      column(width = 9, 
             
             fluidRow(
               #draw the visnetwork
               visNetworkOutput("main_network", height = "600px")
             )
             
      )
      
    ), 
    fluidRow(
      column(width = 3),
      column(width = 3, 
             tags$h4("Number of Wallets:"),
             textOutput("wallet_number")),
      column(width = 3, 
             tags$h4("Blacklisted Wallets:"),
             textOutput("blacklist_number")), 
      column(width = 3, 
             tags$h4("Suspicious Wallets:"),
             textOutput("sus_number"))
       
    ),
    fluidRow(
      wellPanel(

        includeHTML("./www/include_footer.html")
  
      ),
      
    )
    
    
  )
)

