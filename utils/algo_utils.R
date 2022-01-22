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

compute_g <- function(edges) {
  edges_mat <- edges %>% select(from, to) %>% as.matrix(byrow = TRUE)
  g <- igraph::graph_from_edgelist(edges_mat, directed = FALSE)
  g <- igraph::simplify(g, remove.loops = TRUE, remove.multiple = FALSE)
  return(g)
}

#Main function to download all transactions for a given ASA
get_all_tx <- function(ASA_id = "432975976", ncores = 1, confirmed_round = 1) {
  tmp <- tempfile()
  
  test <- try(curl_download(paste0('https://algoindexer.algoexplorerapi.io/v2/assets/', 
                                   ASA_id, '/transactions?min-round=', confirmed_round), tmp))
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
                 '&before-time=', date_i1, '&min-round=', confirmed_round)
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

build_network <- function(df, blacklist =  "data/blacklist.csv", 
                          whitelist = "data/whitelist.csv", 
                          decimal, ASA_id, ncores = 1, 
                          confirmed_round = 1, quick_build = FALSE) {
  
  blacklist <- read.csv(blacklist)$Addresses
  whitelist <- read.csv(whitelist)$Addresses
  
  #if we already did some of the cleaning in init_network
  if(ncol(df) != 7) {
    df <- df %>% 
      clean_names() %>% 
      select(asset_transfer_transaction, sender, confirmed_round) %>% 
      unnest(cols = c(asset_transfer_transaction))
  }
  
  #finish the cleaning
  edges <- df %>%
    mutate(amount = amount/(10^decimal)) %>%
    rename(from = sender,
           to = receiver) %>%
    group_by(from, to) %>% 
    summarise(amount = sum(amount), 
              confirmed_round = max(confirmed_round)) %>% 
    mutate(ASA_id = ASA_id, 
           color = "red")
  edges$value <- scale(edges$amount)[1:nrow(edges)]
  
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
    compute_holdings(decimal = decimal,
                     ASA_id = ASA_id) %>% 
    mutate(group = 
             case_when(id %in% blacklist ~ "blacklist", 
                       id %in% possible_blacklist ~ "suspicious",
                       id %in% whitelist ~ "whitelist",
                       TRUE ~ "clean"), 
           label = group) 
  
  #don't get all algo edges if quick build is true
  if(quick_build) {
    nodes <- nodes %>% filter(id %in% edges$from | id %in% edges$to)
  }
  
  #compute the algo edges --> this takes a minute
  edges <- compute_algo_edges(nodes = nodes, edges = edges, ncores = ncores, 
                              whitelist = whitelist, confirmed_round = confirmed_round)
  #add some key variables
  #add a font size column to the node df to hide labels
  nodes$font.size <- 0
  nodes <- get_wallet_info(nodes)
  
  #get an igraph g
  g <- compute_g(edges)
  return(list(nodes, edges, g))
} 

#function to compute the degree of everything
compute_degree <- function(nodes, edges) {
  #compute g and get the degree of every node
  g <- compute_g(edges)
  degree <- igraph::degree(g)
  degree_df <- data.frame(id = names(degree), degree = degree)
  
  if(any(colnames(nodes) == "degree")) { #we used to compute degree up front so just need to correct for that if so
    nodes <- nodes %>% select(-degree)
  }
  nodes <- left_join(nodes, degree_df) %>% 
    mutate(degree = ifelse(is.na(degree), 0, degree)) 
  
  return(nodes)
}

#Function to compute the edges for algo transactions
compute_algo_edges <- function(nodes, edges, whitelist, ncores, confirmed_round) {
  
  #initialize tempfile for downloads
  tmp <- tempfile()
  
  #Setup to loop through every wallet, grabbing algo transactions
  out_list <- list()
  ids <- nodes$id
  ids <- ids[!(ids %in% whitelist)]
  
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
               '/transactions?tx-type=pay&currency-greater-than=1000000&min-round=', 
               confirmed_round), tmp
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
      #bind all the pages of results together
      #also filter out all transactions where to and from aren't in ids
      df_i <- bind_rows(df_list) %>% filter(sender %in% ids & receiver %in% ids)
      
      
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
  df <- bind_rows(out2) 
  if(nrow(df != 0)) {
    df <- df %>% 
      distinct(id, .keep_all = TRUE) %>%
      rename(from = "sender",
             to = "receiver") %>% 
      group_by(from, to) %>% 
      summarise(amount = sum(amount)/(10^6)) %>%
      mutate(color = "black", 
             ASA_id = NA)
    df$value <- scale(df$amount)[1:nrow(df)]
    
    edges <- bind_rows(edges, df) %>% 
      filter(!is.na(from), !is.na(to))
  } 
  return(edges)
  
}

compute_holdings <- function(ASA_id, decimal = 3) {
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
  
  
  #do some more data cleaning
  balances$amount <- balances$amount/(10^decimal)
  balances <- balances %>% 
    select(address, amount) %>% rename(id = address)
  
  nodes <- balances %>% mutate(value = amount)
  return(nodes)
}

#function to get the number of assets a given wallet is opted into and the wallet age
get_wallet_info <- function(nodes) {
  if(nrow(nodes) == 0) {
    return(nodes)
  }
  tmp <- tempfile()
  ids <- nodes$id
  num_assets <- vector(mode = "numeric", length = length(ids))
  wallet_age <- vector(mode = "numeric", length = length(ids))
  for(i in 1:length(ids)) {
    
    #download string for the algoexplorer api
    curl_download(paste0("https://algoindexer.algoexplorerapi.io/v2/accounts/",
                         ids[i], "?include-all=true"),
                  tmp)
    
    page <- read_json(tmp, simplifyVector = TRUE)
    assets <- page[[1]]$assets
    num_assets[i] <- nrow(assets)
    wallet_age[i] <- get_wallet_age(page)
  }
  
  nodes$num_assets <- num_assets
  nodes$wallet_age <- wallet_age
  return(nodes)
}

get_wallet_age <- function(page) {
  #get round where wallet was created
  round <- page$account$`created-at-round`
  #get current date
  current_round <- page[[2]]
  
  #generate time since creation in days
  wallet_age <- ((current_round - round) * 4.5)/ 60 / 60 / 24
  
  return(wallet_age)

}

filter_network <- function(ASA_id, whitelist, blacklist, min_holding, 
                           minimum_tx, minimum_degree) {
  if(file.exists(paste0("data/", ASA_id, "_network.Rda"))) {
    load(paste0("data/", ASA_id, "_network.Rda"))
  } else {
    stop("ASA_id not found")
  }
  nodes <- out[[1]]
  edges <- out[[2]]
  #Load the blacklist and whitelist
  blacklist <- read.csv(blacklist)$Addresses
  whitelist <- read.csv(whitelist)$Addresses
  
  nodes <- nodes %>% 
    filter(!(id %in% whitelist), 
           amount > min_holding)
  
  #don't filter out the algo transactions - they are on an entirely different scale 
  edges <- edges %>% 
    filter(!(from %in% whitelist), !(to %in% whitelist), 
           amount > minimum_tx & color == "red" | color == "black", 
           from %in% nodes$id & to %in% nodes$id)
  
  #recalculate degree for the new edges
  #also configure the long label
  nodes <- compute_degree(nodes, edges) %>% 
    filter(degree >= minimum_degree) %>% 
    mutate(font.size = 0, 
           wallet_age = round(wallet_age),
           label_long = paste0("Number of Assets: ", num_assets, 
                               " \n Wallet age: ", wallet_age, " days"))
  
  #filter nodes to remove any that aren't the minimum_degree
  
  g <- compute_g(edges)
  return(list(nodes, edges, g))
}

#Main Function that checks it and also the cache
create_network <- function(ASA_id = "432975976", 
                           decimal = 3, 
                           min_holding = 100000,
                           blacklist =  "data/blacklist.csv", 
                           minimum_tx = 1000, 
                           minimum_degree = 1,
                           force_update = FALSE, 
                           ncores = 1) {
  
  #define whitelist based on ASA_id
  whitelist <- paste0("data/", ASA_id, "_whitelist.csv")
  if(file.exists(paste0("data/", ASA_id, "_network.Rda")) & !force_update) {
    out <- filter_network(ASA_id = ASA_id, whitelist = whitelist, 
                          blacklist = blacklist, min_holding = min_holding,
                          minimum_tx = minimum_tx, 
                          minimum_degree = minimum_degree)
    
    return(out)
  } 
  
  
  
  df <- get_all_tx(ASA_id = ASA_id, ncores = ncores)
  out <- build_network(df = df, blacklist = blacklist, whitelist = whitelist,
                       decimal = decimal, ASA_id = ASA_id, ncores = ncores)
  
  save(out, file = paste0("data/", ASA_id, "_network.Rda"))
  
  return(out)
  
}


#Function to add most up to date transactions to transaction network- 
#this should run in a few minutes or less compared to hours for a full download
update_network <- function(ASA_id = "432975976", 
                           decimal = 3, 
                           blacklist =  "data/blacklist.csv", 
                           ncores = 1, quick_build = TRUE) {
  
  whitelist <- paste0("data/", ASA_id, "_whitelist.csv")
  #load current network
  load(paste0("data/", ASA_id, "_network.Rda"))
  #extract current nodes and edges
  nodes <- out[[1]] 
  if(ncol(nodes) %in% c(6,7)) {
    nodes <- get_wallet_info(nodes)
  }
  edges <- out[[2]] %>% 
    mutate(ASA_id = as.numeric(ASA_id))
  #grab the last known round
  last_round <- max(edges$confirmed_round, na.rm = TRUE)
  #get all transactions for the ASA since the last known round
  df <- get_all_tx(ASA_id = ASA_id, ncores = ncores, confirmed_round = last_round)
  
  #do all the required post-processing and additional steps to create networks from all transactions since the last known round.
  out <- build_network(df, decimal = decimal, blacklist = blacklist, 
                       whitelist = whitelist, ncores = 4, 
                       confirmed_round = last_round, quick_build = quick_build,
                       ASA_id = ASA_id)
  
  #bind the new and old nodes together
  new_nodes <- out[[1]] %>% filter(!(id %in% nodes$id)) %>% get_wallet_info()
  nodes <- bind_rows(new_nodes, nodes)
  new_edges <- out[[2]] %>% 
    mutate(ASA_id = as.numeric(ASA_id))

  #re-do the processing to incorporate the new transactions
  edges <- bind_rows(edges, new_edges) %>% 
    group_by(from, to, ASA_id, color) %>% 
    summarise(amount = sum(amount), 
              confirmed_round = max(confirmed_round)) %>% 
    ungroup()
  edges$value <- scale(edges$amount)[1:nrow(edges)]
  
  
  g <- compute_g(edges)
  out <- list(nodes, edges, g) 
  
  save(out, file = paste0("data/", ASA_id, "_network.Rda"))
}


asa_index <- data.frame(asa_name = c("Commie Coin (USSR)",
                                     "BirdBot (BIRDS)",
                                     "AlgoMeow (MEOW)",
                                     "CryptoRulesEverythingAroundMe (CREAM)",
                                     "Parsec (PRSC)",
                                     "Parsec AU (PRSCau)", 
                                     "LOUDefi (LOUD)"),
                        asa_id = c(432975976, 478549868, 361806984, 312412702,
                                   415045633, 546713076, 457819394),
                        decimal = c(3, 0, 0, 6, 0, 6, 6),
                        min_holding = c(200000, 10000, 10000, 10000, 10000, 10000, 10000),
                        minimum_tx = c(0,0,0,0,0,0,0))


out = create_network()
nodes_init <- out[[1]]
edges_init <- out[[2]] 
ledges <- data.frame(color = c("red", "black"),
                     label = c("ASA", "Algo"), arrows =c("to"), 
                     font.size = c(16), 
                     width = 4, length = 10)
g_init <- out[[3]]



