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
library(shinyWidgets)

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
        style='border-bottom: 1px solid black; height:80px',
        tags$h3(tags$a(href = "mailto:davis@block-rank.io", "Contact us"))
      ),
      column( 
        width = 4, 
        style='border-bottom: 1px solid black; height:80px',
        tags$h1(tags$img(src = "br_title.png", width = '280px', height = '50px'), 
                align = "center")
      ),
      column(
        width = 4, 
        style='border-bottom: 1px solid black; height:80px',
        # tags$img(src = "birdbot.jpeg", width = "40px", height = "55px",
        #          align = "right")
        tags$h3(tags$a(href="https://www.youtube.com/watch?v=0s2u_U9JxUI&t=2s&ab_channel=BlockRank", "How does this work?"), 
                align = "right")
      )
    ),
    
    fluidRow(
      column(width = 3, style='border-right: 1px solid black',
             fluidRow(
               column(width = 10, offset = 1,
                      selectInput("asa_id", "ASA ID:", 
                                  c("BirdBot (BIRDS)",
                                    "AlgoMeow (MEOW)",
                                    "CryptoRulesEverythingAroundMe (CREAM)",
                                    "Commie Coin (USSR)",
                                    "Parsec (PRSC)",
                                    "Parsec AU (PRSCau)"))
                                   # "LOUDefi (LOUD)"))
               )
             ),
             fluidRow(
               column(width = 10, offset = 1,
                      #mess around with different network filtering stuff
                      tags$br(),
                      searchInput('wallet_search', label = "Wallet Search",
                                  btnSearch = icon("search")),
                      tags$br(),
                      numericInput("min_holding", "Minimum ASA Holding", value = 200000, min = 0),
                      tags$br(),
                      actionButton("add_sus", "Add Suspicious Wallets to Blacklist"),
                      tags$br(),
                      tags$br(),
                      actionButton("update_networks", "Fetch most recent transactions"), #comment this out for the public version
                      tags$br(),
                      tags$br(),
                      downloadButton("download", "Download Current Blacklist"),
                      tags$br(),
                      tags$br(),
                      actionButton("clear_blacklist", "Clear current blacklist")
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

