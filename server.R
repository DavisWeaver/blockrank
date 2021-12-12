library(shiny)
library(visNetwork)
source("utils.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  graph_data <- reactiveValues(
   
    nodes = nodes_init,
    edges = edges_init,
    wallet_number = nrow(nodes_init),
    scam_wallets = sum(nodes_init$group == "blacklist"), 
    sus_wallets = sum(nodes_init$group == "suspicious"),
    current_blacklist = nodes_init %>% filter(group == "blacklist") %>% 
      select(id)
  )
  
  #define wallet count outputs
  output$wallet_number <- renderText(graph_data$wallet_number)
  output$blacklist_number <- renderText(graph_data$scam_wallets)
  output$sus_number <- renderText(graph_data$sus_wallets)
  
  #just for debugging
  output$all_nodes = renderTable({
    graph_data$nodes
  })
  
  #initialize a new graph when you change the ASA_id
  observeEvent(input$asa_id, {
    id_df = asa_index %>% filter(asa_name == input$asa_id)
    id = id_df$asa_id
    #construct network for asa_id-  need to cache this I would think
    out = create_network(ASA_id = id)
    
    #update all the reactive values
    graph_data$nodes = out[[1]]
    graph_data$edges = out[[2]]
    graph_data$wallet_number = nrow(graph_data$nodes)
    graph_data$scam_wallets = sum(graph_data$nodes$group == "blacklist") 
    graph_data$sus_wallets = sum(graph_data$nodes$group == "suspicious")
    
    output$main_network <- renderVisNetwork({
      visNetwork(graph_data$nodes, graph_data$edges) %>%
        visGroups(groupname = "blacklist", color = "black") %>%    # darkblue for group "A"
        visGroups(groupname = "suspicious", color = "red") %>%
        visGroups(groupname = "clean", color = "blue") %>%
        visOptions(selectedBy = "group", 
                   manipulation = TRUE) 
    })
    
    visNetworkProxy("main_network") %>%
      visRedraw()
    
  })
  
  #this will 
  observeEvent(input$main_network_graphChange, {
    # If the user added a node, add it to the data frame of nodes.
    # If the user edited a node, update that record.
    if(input$main_network_graphChange$cmd == "editNode") {
      temp = graph_data$nodes
      temp$label[temp$id == input$main_network_graphChange$id] = input$main_network_graphChange$label
      graph_data$nodes = temp
      graph_data$nodes$group <- graph_data$nodes$label
      visNetworkProxy("main_network") %>% 
        visUpdateNodes(nodes = input$main_network_graphChange) 
      
      #update reactive values
      graph_data$scam_wallets = sum(graph_data$nodes$group == "blacklist") 
      graph_data$sus_wallets = sum(graph_data$nodes$group == "suspicious")
      graph_data$current_blacklist = graph_data$nodes %>% filter(group == "blacklist") %>% 
        select(id)
    }
    
  })
  
  output$download <- downloadHandler(
    filename = function () {
      paste0("current_blacklist_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(graph_data$current_blacklist, file)
    }
  )
  
  
  
  
})
