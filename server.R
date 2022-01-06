library(shiny)
library(visNetwork)
source("utils.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  graph_data <- reactiveValues(
    nodes = nodes_init,
    edges = edges_init,
    max_node_value = max(nodes_init$amount),
    wallet_number = nrow(nodes_init),
    scam_wallets = sum(nodes_init$group == "blacklist"), 
    sus_wallets = sum(nodes_init$group == "suspicious"),
    current_blacklist = nodes_init %>% filter(group == "blacklist") %>% 
      select(id), 
    g = g_init
  )
  
  #define wallet count outputs
  output$wallet_number <- renderText(graph_data$wallet_number)
  output$blacklist_number <- renderText(graph_data$scam_wallets)
  output$sus_number <- renderText(graph_data$sus_wallets)
  
  #initialize a new graph when you change the ASA_id
  observeEvent(input$asa_id, {
    id_df = asa_index %>% filter(asa_name == input$asa_id)
    id = id_df$asa_id
    
    #construct network for asa_id-  need to cache this I would think
    out = create_network(ASA_id = id, min_holding = id_df$min_holding, 
                         decimal = id_df$decimal, 
                         minimum_tx = id_df$minimum_tx)
    
    #update all the reactive values
    graph_data$nodes = out[[1]]
    graph_data$max_node_value = max(out[[1]]$amount)
    graph_data$edges = out[[2]] 
    graph_data$g = out[[3]]
    graph_data$wallet_number = nrow(graph_data$nodes)
    graph_data$scam_wallets = sum(graph_data$nodes$group == "blacklist") 
    graph_data$sus_wallets = sum(graph_data$nodes$group == "suspicious")
    
    #need a version of edges that isn't reactive
    
    output$main_network <- renderVisNetwork({
      visNetwork(out[[1]], out[[2]]) %>%
        visEdges(smooth = FALSE,
                 arrows = list(to = list(enabled = TRUE, scaleFactor = 1))) %>%
        visGroups(groupname = "blacklist", color = "black") %>%    # darkblue for group "A"
        visGroups(groupname = "suspicious", color = "red") %>%
        visGroups(groupname = "clean", color = "blue") %>%
        visOptions(selectedBy = "group", 
                   manipulation = TRUE) %>% 
        visLegend(width = 0.15, position = "right", 
                  addEdges = ledges, useGroups = FALSE)
    })
    
    visNetworkProxy("main_network") %>%
      visRedraw()
    
  })
  
  observeEvent(input$min_holding, {
    if(input$min_holding > graph_data$max_node_value) {
      showNotification("No nodes meet these criteria, defaulting to previous value")
    } else {
      
      id_df = asa_index %>% filter(asa_name == input$asa_id)
      id = id_df$asa_id
      
      #construct network for asa_id-  need to cache this I would think
      out2 = create_network(ASA_id = id, min_holding = input$min_holding, 
                            decimal = id_df$decimal, 
                            minimum_tx = id_df$minimum_tx)
      
      #this will fly an error if there are no nodes or edges that match whatever condition
      nodes_prev <- graph_data$nodes
      edges_prev <- graph_data$edges
      #update all the reactive values
      graph_data$nodes = out2[[1]]
      graph_data$edges = out2[[2]] 
      graph_data$g = out2[[3]]
      graph_data$wallet_number = nrow(graph_data$nodes)
      graph_data$scam_wallets = sum(graph_data$nodes$group == "blacklist") 
      graph_data$sus_wallets = sum(graph_data$nodes$group == "suspicious")
      
      #Add nodes and edges if we made it more inclusive
      if(nrow(out[[1]]) > nrow(nodes_prev)) {
        visNetworkProxy("main_network") %>%
          visUpdateNodes(out2[[1]]) %>% 
          visUpdateEdges(out2[[2]])
      } else {
        nodes_remove <- nodes_prev %>% filter(!(id %in% graph_data$nodes$id))
        visNetworkProxy("main_network") %>%
          visRemoveNodes(nodes_remove$id) %>% 
          visRemoveEdges(nodes_remove$id)
      }
    }
    
    
    
    
    
  })
  #this will 
  observeEvent(input$main_network_graphChange, {
    # If the user added a node, add it to the data frame of nodes.
    # If the user edited a node, update that record.
    if(input$main_network_graphChange$cmd == "editNode") {
      temp = graph_data$nodes
      temp$label[temp$id == input$main_network_graphChange$id] = input$main_network_graphChange$label
      
      #get neighbors if they just blacklisted something
      if(input$main_network_graphChange$label == "blacklist") {
        neighbors <- names(igraph::neighbors(graph_data$g, v = input$main_network_graphChange$id))
        temp <- temp %>% 
          mutate(label = ifelse(id %in% neighbors, "suspicious", label))
      }
      graph_data$nodes = temp
      graph_data$nodes$group <- graph_data$nodes$label
      visNetworkProxy("main_network") %>%
        visUpdateNodes(nodes = graph_data$nodes)
      
      #update reactive values
      graph_data$scam_wallets = sum(graph_data$nodes$group == "blacklist") 
      graph_data$sus_wallets = sum(graph_data$nodes$group == "suspicious")
      graph_data$current_blacklist = graph_data$nodes %>% filter(group == "blacklist") %>% 
        select(id)
    }
    
  })
  
  observeEvent(input$add_sus, {
    #blacklist all suspicious nodes
    graph_data$nodes <- graph_data$nodes %>% 
      mutate(label = ifelse(label == "suspicious", "blacklist", label), 
             group = label)
    
    graph_data$current_blacklist = graph_data$nodes %>% filter(group == "blacklist") %>% 
      select(id)
    
    blacklist <- graph_data$current_blacklist$id
    
    #now update suspicious nodes
    possible_blacklist <- graph_data$edges %>% 
      filter(from %in% blacklist | to %in% blacklist)
    
    #grab a possible blacklist of all wallets with a transaction including the banned wallets
    possible_blacklist <- unique(c(possible_blacklist$from, possible_blacklist$to))
    #exclude whitelist and original blacklist to make this a unique identifier
    possible_blacklist <- possible_blacklist[!(possible_blacklist %in% blacklist)]
    
    #update sus
    graph_data$nodes <- graph_data$nodes %>% 
      mutate(label = ifelse(id %in% possible_blacklist, "suspicious", label),
             group = label)
    
    visNetworkProxy("main_network") %>%
      visUpdateNodes(nodes = graph_data$nodes)
    
    #update reactive values
    graph_data$scam_wallets = sum(graph_data$nodes$group == "blacklist") 
    graph_data$sus_wallets = sum(graph_data$nodes$group == "suspicious")
    graph_data$current_blacklist = graph_data$nodes %>% filter(group == "blacklist") %>% 
      select(id)
  }) 
  
  output$download <- downloadHandler(
    filename = function () {
      paste0("current_blacklist_", Sys.Date(), ".ecsv")
    },
    content = function(file) {
      write.csv(graph_data$current_blacklist, file)
    }
  )
  
})
