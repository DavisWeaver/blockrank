library(curl)
library(jsonlite)
library(magrittr)
library(dplyr)
library(janitor)
library(tidyr)
library(igraph)


get_all_tx <- function(ASA_id = "432975976") {
  tmp <- tempfile()
  curl_download(paste0('https://algoexplorerapi.io/idx2/v2/assets/', 
                       ASA_id, '/transactions?currency-greater-than=10000'), tmp)
  page <- read_json(tmp, simplifyVector = TRUE)
  df_list <- list()
  i = 1
  df_list[[i]] <- page[[3]] #get the first one going
  while(nrow(page[[3]]) == 1000) { #1000 is the max per page 
    i <- i + 1
    curl_download(paste0('https://algoexplorerapi.io/idx2/v2/assets/', 
                         ASA_id, '/transactions?next=', page[[2]]), tmp)
    page <- read_json(tmp, simplifyVector = TRUE)
    df_list[[i]] <- page[[3]]
  }
  df <- bind_rows(df_list)
  return(df)
}
#df <- get_all_tx()
#save(df, file = "./data/USSR_tx_all.Rda")

init_network <- function(df, blacklist =  "data/blacklist.csv", 
                         whitelist = "data/whitelist.csv", 
                         minimum_tx = 10000000, 
                         min_holding,
                         decimal, 
                         minimum_degree, ASA_id) {
  
  blacklist <- read.csv(blacklist)$Addresses
  whitelist <- read.csv(whitelist)$Addresses
  edges <- df %>% 
    clean_names() %>% 
    select(asset_transfer_transaction, sender) %>% 
    unnest(cols = c(asset_transfer_transaction)) %>% 
    rename(from = sender,
           to = receiver) %>% 
    filter(!(from %in% whitelist), !(to %in% whitelist)) %>%
    group_by(from, to) %>% 
    summarise(amount = sum(amount)) %>% 
    filter(amount > minimum_tx) %>% 
    mutate(width = log10(amount))
  
  #create a possible blacklist based on nodes that transact with the actual blacklist
  possible_blacklist <- edges %>% 
    filter(from %in% blacklist | to %in% blacklist)
  
  #grab a possible blacklist of all wallets with a transaction including the banned wallets
  possible_blacklist <- unique(c(possible_blacklist$from, possible_blacklist$to))
  #exclude whitelist and original blacklist to make this a unique identifier
  possible_blacklist <- possible_blacklist[!(possible_blacklist %in% blacklist) & !(possible_blacklist %in% whitelist)]
  
  id = unique(c(edges$from, edges$to))
  
  #compute the holding using the API
  nodes <- 
    compute_holdings(decimal = decimal, min_holding = min_holding,
                     ASA_id = ASA_id) %>% 
    mutate(group = 
             case_when(id %in% blacklist ~ "blacklist", 
                       id %in% possible_blacklist ~ "suspicious", 
                       TRUE ~ "clean"), 
           label = group) %>% 
    filter(!(id %in% whitelist))
  edges <- edges %>% filter(from %in% nodes$id & to %in% nodes$id)
  #add some key variables
  nodes <- compute_degree(nodes, edges) %>% 
    mutate(degree = ifelse(is.na(degree), 0, degree))
  #only keep nodes w/ a certain minimum degree
  nodes <- nodes %>% filter(degree >= minimum_degree)
  
  #add a font size column to the node df to hide labels
  nodes$font.size <- 0
  
  return(list(nodes, edges))
} 

#function to compute the degree of everything
compute_degree <- function(nodes, edges) {
  #compute g and get the degree of every node
  edges <- edges %>% select(from, to) %>% as.matrix()
  g <- igraph::graph_from_edgelist(edges)
  degree <- igraph::degree(g)
  degree_df <- data.frame(id = names(degree), degree = degree)
  nodes <- left_join(nodes, degree_df)
  return(nodes)
}

compute_holdings <- function(ASA_id, min_holding = 10000, decimal = 3) {
  tmp <- tempfile()
  curl_download(paste0('https://algoexplorerapi.io/idx2/v2/assets/', 
                       ASA_id, '/balances?'), tmp)
  page <- read_json(tmp, simplifyVector = TRUE)
  
  df_list <- list()
  df_list[[1]] <- page[[1]]
  i = 1
  while(nrow(page[[1]]) == 1000) { #1000 is the max per page 
    i <- i + 1
    curl_download(paste0('https://algoexplorerapi.io/idx2/v2/assets/', 
                         ASA_id, '/balances?next=', page[[3]]), tmp)
    page <- read_json(tmp, simplifyVector = TRUE)
    df_list[[i]] <- page[[1]]
  }
  balances <- bind_rows(df_list)
  
  balances$amount <- balances$amount/(10^decimal)
  balances <- balances %>% filter(amount > min_holding) %>% 
    select(address, amount) %>% rename(id = address)
  
  nodes <- balances %>% mutate(value = amount)
  return(nodes)
}

#Main Function that checks it and also the cache
create_network <- function(ASA_id = "432975976", 
                           decimal = 3, 
                           min_holding = 100000,
                           blacklist =  "data/blacklist.csv", 
                           minimum_tx = 10000000, 
                           minimum_degree = 2,
                           force_update = FALSE) {
  
  if(file.exists(paste0("data/", ASA_id, "_network.Rda")) & !force_update) {
    load(paste0("data/", ASA_id, "_network.Rda"))
    return(out)
  } 
  
  #define whitelist based on ASA_id
  whitelist <- paste0("data/", ASA_id, "_whitelist.csv")
  
  df <- get_all_tx(ASA_id = ASA_id)
  out <- init_network(df = df, blacklist = blacklist, whitelist = whitelist,
                      minimum_tx = minimum_tx, min_holding = min_holding, 
                      decimal = decimal, 
                      minimum_degree = minimum_degree, ASA_id = ASA_id)
  
  save(out, file = paste0("data/", ASA_id, "_network.Rda"))
  
  return(out)
  
}

asa_index <- data.frame(asa_name = c("Commie Coin (USSR)", "AlgoMeow (MEOW)"), 
                        asa_id = c(432975976, 361806984))

out = create_network()
# out = create_network(minimum_degree = 1, minimum_tx = 10000,
#                      min_holding= 0, force_update = TRUE)
# out = create_network(ASA_id = "361806984", decimal = 0, force_update = TRUE,
#                       minimum_tx = 50, min_holding = 0, minimum_degree = 1)
nodes_init <- out[[1]]
edges_init <- out[[2]]



