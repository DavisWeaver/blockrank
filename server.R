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
      select(id), 
    g = g_init
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
    graph_data$g = out[[3]]
    graph_data$wallet_number = nrow(graph_data$nodes)
    graph_data$scam_wallets = sum(graph_data$nodes$group == "blacklist") 
    graph_data$sus_wallets = sum(graph_data$nodes$group == "suspicious")
    
    output$main_network <- renderVisNetwork({
      visNetwork(graph_data$nodes, graph_data$edges) %>%
        visEdges(smooth = FALSE) %>%
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
      paste0("current_blacklist_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(graph_data$current_blacklist, file)
    }
  )
  
  
  
  
})
