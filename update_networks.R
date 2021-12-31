library(foreach)
library(dplyr)
library(curl)
library(jsonlite)
library(magrittr)
library(janitor)
library(tidyr)
library(igraph)
source("utils.R")
update_networks <- function(asa_index, ncores) {
  out <- foreach(i = 1:nrow(asa_index), 
                 .packages = c("dplyr", "magrittr", "igraph", "tidyr",
                               "janitor", "jsonlite", "curl"), 
                 .export = c("create_network", "get_all_tx", "init_network", 
                             "compute_degree", "compute_holdings", 
                             "generate_daterange")) %do% {
    asa_i <- slice(asa_index, i)
    out = create_network(ASA_id = asa_i$asa_id, minimum_degree = 1, minimum_tx = 10,
                         min_holding= asa_i$min_holding, force_update = TRUE,
                         decimal = asa_i$decimal, ncores = ncores)
    return(NULL)
  }
}

# asa_index <- data.frame(asa_name = c("Commie Coin (USSR)","BirdBot (BIRDS)",
#                                      "Akita Inu (AKITA)",
#                                      "AlgoMeow (MEOW)",
#                                      "Svansy Coin (SVANSY)", "MoonX (MOONX)",
#                                      "Matrix (MTRX)", "CryptoRulesEverythingAroundMe (CREAM)"),
#                         asa_id = c(432975976, 478549868, 384303832, 361806984, 388502764, 404719435, 234994096, 312412702),
#                         decimal = c(3,0, 0, 0, 6,5, 0, 6))

asa_index <- data.frame(asa_name = c("Commie Coin (USSR)",
                                     "BirdBot (BIRDS)",
                                     "AlgoMeow (MEOW)",
                                     "Svansy Coin (SVANSY)", "MoonX (MOONX)",
                                     "Matrix (MTRX)",
                                     "CryptoRulesEverythingAroundMe (CREAM)"),
                        asa_id = c(432975976, 478549868, 361806984, 388502764, 404719435,
                                   234994096, 312412702),
                        decimal = c(3, 0, 0, 6,5, 0, 6), 
                        min_holding = c(200000, 10000, 100000, 10000000, 100000, 10000000, 10000))

update_networks(asa_index = asa_index, ncores = 6)
