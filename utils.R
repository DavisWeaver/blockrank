library(curl)
library(jsonlite)
library(magrittr)
library(dplyr)
library(janitor)
library(tidyr)
library(igraph)
library(lubridate)
library(foreach)

#convenience function for generating a range of dates
generate_daterange <- function() {
  date_init <- lubridate::as_datetime(Sys.time() - 13000000)
  date_stop <- lubridate::as_datetime(Sys.time())
  date_range <- seq(date_init, date_stop, by = "2 days")
  return(date_range)
}

#Main function to download all transactions for a given ASA
get_all_tx <- function(ASA_id = "432975976", ncores = 1) {
  tmp <- tempfile()
  
  test <- try(curl_download(paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                                   ASA_id, '/transactions'), tmp))
  if(inherits(test, "try-error")) {
    #try backup pipeline for large numbers of transactions ASAs
    date_range <- generate_daterange()
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    out <- foreach(i = 1:length(date_range), 
                   .packages = c("dplyr", "magrittr", "igraph", "tidyr",
                                 "janitor", "jsonlite", "curl"), 
                   .export = c( "generate_daterange"), 
                   .errorhandling = "pass") %dopar% 
      {
        #don't run the last iteration of the loop
        if(i == length(date_range)) {
          return()
        }
        date_i <- format(date_range[i], "%Y-%m-%dT%H:%M:%SZ")
        date_i1 <- format(date_range[i+1], "%Y-%m-%dT%H:%M:%SZ")
        #format the download string
        
        download_string_i <- 
          paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                 ASA_id, '/transactions?after-time=', date_i,
                 '&before-time=', date_i1)
        #try the damn thing twice and no more
        test <- try(curl_download(download_string_i, tmp))
        if(inherits(test, "try-error")) {
          curl_download(download_string_i, tmp)
        }
        page <- read_json(tmp, simplifyVector = TRUE)
        #don't start the whole next thing until we find transactions
        if(length(page) <3) {
          return()
        }
        
        page_df <- page[[3]] %>% 
          clean_names() %>% 
          select(asset_transfer_transaction, sender, confirmed_round) %>% 
          unnest(cols = c(asset_transfer_transaction))
        df_list <- list()
        j = 1
        df_list[[j]] <- page_df #get the first one going
        while(nrow(page[[3]]) == 1000) { #1000 is the max per page 
          j <- j + 1
          curl_download(paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                               ASA_id, '/transactions?next=', page[[2]]), tmp)
          
          page <- read_json(tmp, simplifyVector = TRUE)
          page_df <- page[[3]] %>% 
            clean_names() %>% 
            select(asset_transfer_transaction, sender, confirmed_round) %>% 
            unnest(cols = c(asset_transfer_transaction))
          df_list[[j]] <- page_df
        }
        df_i <- bind_rows(df_list)
        return(df_i)
      }
    out2 <- list()
    for(i in 1:length(out)) {
      if(is.data.frame(out[[i]])) {
        out2[[i]] <- out[[i]]
      }
    }
    df <- bind_rows(out2)
    
  } else {
    page <- read_json(tmp, simplifyVector = TRUE)
    df_list <- list()
    i = 1
    df_list[[i]] <- page[[3]] #get the first one going
    while(nrow(page[[3]]) == 1000) { #1000 is the max per page 
      i <- i + 1
      test <- try(curl_download(paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                                       ASA_id, '/transactions?next=', page[[2]]), tmp))
      while(inherits(test, "try-error")) {
        test <- try(curl_download(paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                                         ASA_id, '/transactions?next=', page[[2]]), tmp))
      }
      
      page <- read_json(tmp, simplifyVector = TRUE)
      df_list[[i]] <- page[[3]]
    }
    df <- bind_rows(df_list) 
  }
  
  return(df)
}
#df <- get_all_tx()
#save(df, file = "./data/USSR_tx_all.Rda")

init_network <- function(df, blacklist =  "data/blacklist.csv", 
                         whitelist = "data/whitelist.csv", 
                         minimum_tx = 10000000, 
                         min_holding,
                         decimal, 
                         minimum_degree, ASA_id, ncores = 1) {
  
  blacklist <- read.csv(blacklist)$Addresses
  whitelist <- read.csv(whitelist)$Addresses
  
  #if we already did some of the cleaning in init_network
  if(ncol(df) == 7) {
    edges <- df %>%
      mutate(amount = amount/(10^decimal)) %>%
      rename(from = sender,
             to = receiver) %>% 
      filter(!(from %in% whitelist), !(to %in% whitelist)) %>%
      group_by(from, to) %>% 
      summarise(amount = sum(amount), 
                confirmed_round = max(confirmed_round)) %>% 
      filter(amount > minimum_tx) %>% 
      mutate(color = "red")
    edges$value <- scale(edges$amount)[1:nrow(edges)]
  } else {
    edges <- df %>% 
      clean_names() %>% 
      select(asset_transfer_transaction, sender, confirmed_round) %>% 
      unnest(cols = c(asset_transfer_transaction)) %>%
      mutate(amount = amount/(10^decimal)) %>%
      rename(from = sender,
             to = receiver) %>% 
      filter(!(from %in% whitelist), !(to %in% whitelist)) %>%
      group_by(from, to) %>% 
      summarise(amount = sum(amount),
                confirmed_round = max(confirmed_round)) %>% 
      filter(amount > minimum_tx) %>% 
      mutate(color = "red")
    edges$value <- scale(edges$amount)[1:nrow(edges)]
  }
  
  
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
  
  
  #compute the algo edges --> this takes a minute
  edges <- compute_algo_edges(nodes = nodes, edges = edges, ncores = ncores, whitelist = whitelist)
  #add some key variables
  nodes <- compute_degree(nodes, edges) %>% 
    mutate(degree = ifelse(is.na(degree), 0, degree))
  
  #want to compute the algo edges/ node info here as well.
  
  #only keep nodes w/ a certain minimum degree
  nodes <- nodes %>% filter(degree >= minimum_degree)
  #add a font size column to the node df to hide labels
  nodes$font.size <- 0
  
  #get an igraph g
  edges_mat <- edges %>% select(from,to) %>% as.matrix()
  g <- igraph::graph_from_edgelist(edges_mat, directed = FALSE)
  return(list(nodes, edges, g))
} 

#function to compute the degree of everything
compute_degree <- function(nodes, edges) {
  #compute g and get the degree of every node
  edges <- edges %>% select(from, to) %>% as.matrix()
  g <- igraph::graph_from_edgelist(edges, directed = FALSE)
  degree <- igraph::degree(g)
  degree_df <- data.frame(id = names(degree), degree = degree)
  nodes <- left_join(nodes, degree_df)
  return(nodes)
}

#Function to compute the edges for algo transactions
compute_algo_edges <- function(nodes, edges, whitelist, ncores) {
  
  #initialize tempfile for downloads
  tmp <- tempfile()
  
  #Setup to loop through every wallet, grabbing algo transactions
  out_list <- list()
  ids <- nodes$id
  
  #setup cluster
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  out <- foreach(i = 1:length(ids), 
                 .packages = c("dplyr", "magrittr", "igraph", "tidyr",
                               "janitor", "jsonlite", "curl"), 
                 .errorhandling = "pass") %dopar% 
    {
      curl_download(
        paste0('https://algoindexer.algoexplorerapi.io/v2/accounts/', ids[i], 
               '/transactions?tx-type=pay&currency-greater-than=1000000'), tmp
      )
      
      #Get the first page of results
      page <- read_json(tmp, simplifyVector = TRUE)
      if(length(page) < 3) {
        return()
      }
      page_df <- page[[3]] %>% 
        clean_names() %>% 
        unnest(cols = c(payment_transaction)) %>%
        select(sender, receiver, amount, id) 
      
      
      #If there is a next token we need to go retrieve the other pages of results
      df_list <- list()
      j = 1
      df_list[[j]] <- page_df #get the first one going
      while(nrow(page_df) == 1000) { #1000 is the max per page 
        j <- j + 1
        curl_download(
          paste0('https://algoindexer.algoexplorerapi.io/v2/accounts/', ids[i], 
                 '/transactions?next=', page[[2]], '&tx-type=pay&currency-greater-than=1000000'), tmp
        )
        
        #download the next page and so on and so on
        page <- read_json(tmp, simplifyVector = TRUE)
        page_df <- page[[3]] %>% 
          clean_names() %>% 
          unnest(cols = c(payment_transaction)) %>%
          select(sender, receiver, amount, id)
        df_list[[j]] <- page_df
      }
      #bind all the pages of results together and filter out transactions involving whitelisted wallets
      #also filter out all transactions where to and from aren't in ids
      df_i <- bind_rows(df_list) %>% filter(!(sender %in% whitelist), 
                                            !(receiver %in% whitelist), 
                                            sender %in% ids & receiver %in% ids)
      
      
      return(df_i)
    }
  
  
  #process the gross output
  out2 <- list()
  for(i in 1:length(out)) {
    if(is.data.frame(out[[i]])) {
      out2[[i]] <- out[[i]]
    }
  }
  
  #finish cleaning + prepping to graph
  df <- bind_rows(out2) %>% 
    distinct(id, .keep_all = TRUE) %>%
    rename(from = "sender",
           to = "receiver") %>% 
    group_by(from, to) %>% 
    summarise(amount = sum(amount)/(10^6)) %>%
    mutate(color = "black")
  
  df$value <- scale(df$amount)[1:nrow(df)]
  
  edges <- bind_rows(edges, df)
  return(edges)
  
}

compute_holdings <- function(ASA_id, min_holding = 10000, decimal = 3) {
  tmp <- tempfile()
  
  test <- try(curl_download(paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                                   ASA_id, '/balances?'), tmp))
  page <- read_json(tmp, simplifyVector = TRUE)
  
  df_list <- list()
  df_list[[1]] <- page[[1]]
  i = 1
  while(nrow(page[[1]]) == 1000) { #1000 is the max per page 
    i <- i + 1
    test <- try(curl_download(paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                                     ASA_id, '/balances?next=', page[[3]]), tmp))
    while(inherits(test, "try-error")) {
      test <- try(curl_download(paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                                       ASA_id, '/balances?next=', page[[3]]), tmp))
    }
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
                           minimum_degree = 1,
                           force_update = FALSE, 
                           ncores = 1) {
  
  if(file.exists(paste0("data/", ASA_id, "_network.Rda")) & !force_update) {
    load(paste0("data/", ASA_id, "_network.Rda"))
    return(out)
  } 
  
  #define whitelist based on ASA_id
  whitelist <- paste0("data/", ASA_id, "_whitelist.csv")
  
  df <- get_all_tx(ASA_id = ASA_id, ncores = ncores)
  out <- init_network(df = df, blacklist = blacklist, whitelist = whitelist,
                      minimum_tx = minimum_tx, min_holding = min_holding, 
                      decimal = decimal, 
                      minimum_degree = minimum_degree, ASA_id = ASA_id, ncores = ncores)
  
  save(out, file = paste0("data/", ASA_id, "_network.Rda"))
  
  return(out)
  
}

asa_index <- data.frame(asa_name = c("Commie Coin (USSR)","BirdBot (BIRDS)",
                                     "Akita Inu (AKITA)",
                                     "AlgoMeow (MEOW)",
                                     "Svansy Coin (SVANSY)", "MoonX (MOONX)",
                                     "Matrix (MTRX)", 
                                     "CryptoRulesEverythingAroundMe (CREAM)"),
                        asa_id = c(432975976, 478549868, 384303832, 361806984, 
                                   388502764, 404719435, 234994096, 312412702),
                        decimal = c(3,0, 0, 0, 6,5, 0, 6))


out = create_network()
nodes_init <- out[[1]]
edges_init <- out[[2]] 
ledges <- data.frame(color = c("red", "black"),
                               label = c("ASA", "Algo"), arrows =c("to"), 
                     font.size = c(16), 
                     width = 4, length = 10)
g_init <- out[[3]]



