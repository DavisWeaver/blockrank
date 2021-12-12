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
ui <- shinyUI(fluidPage(
  withMathJax(),
  # Application title
  
  fluidRow(
    column(
      width = 3,
      style='border-bottom: 1px solid black; height:60px',
      tags$h3(em("Commie Coin (USSR)"))
    ),
    column( 
      width = 6, 
      style='border-bottom: 1px solid black; height:60px',
      tags$h1(strong("BlockRank"), align = "center")
    ),
    column(
      width = 3, 
      style='border-bottom: 1px solid black; height:60px',
      img(src = "hammer01.png", style = "width: 50px")
    )
  ),
  
  fluidRow(
    column(width = 4, style='border-right: 1px solid black',
           fluidRow(
             column(width = 10, offset = 1,
                    selectInput("asa_id", "ASA ID:", 
                                c("Commie Coin (USSR)", "AlgoMeow (MEOW)"))
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
                    downloadButton("download", "Download Current Blacklist")
             )
           )
    ),
    # which node did we delete? just to remind people 
    column(width = 8, 
           
           fluidRow(
             #draw the visnetwork
             visNetworkOutput("main_network", height = "600px")
           )
           
    )
    
  )
)


)

