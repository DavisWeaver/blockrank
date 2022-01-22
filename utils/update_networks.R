library(foreach)
library(dplyr)
library(curl)
library(jsonlite)
library(magrittr)
library(janitor)
library(tidyr)
library(igraph)
source("utils/utils.R")

update_networks <- function(asa_index, ncores, quick_build = TRUE) {
  out <- foreach(i = 1:nrow(asa_index), 
                 .packages = c("dplyr", "magrittr", "igraph", "tidyr",
                               "janitor", "jsonlite", "curl"), 
                 .export = c("create_network", "get_all_tx", "init_network", 
                             "compute_degree", "compute_holdings", 
                             "generate_daterange")) %do%
    {
      
      asa_i <- slice(asa_index, i)
      if(file.exists(paste0("data/", asa_i$asa_id, "_network.Rda"))) {
        update_network(ASA_id = asa_i$asa_id, decimal = asa_i$decimal, ncores = ncores, 
                       quick_build = quick_build)
      } else {
        out = create_network(ASA_id = asa_i$asa_id, force_update = TRUE,
                             decimal = asa_i$decimal, ncores = ncores)
        return(NULL)
      }
    }
}

# asa_index <- data.frame(asa_name = c("Commie Coin (USSR)","BirdBot (BIRDS)",
#                                      "Akita Inu (AKITA)",
#                                      "AlgoMeow (MEOW)",
#                                      "Svansy Coin (SVANSY)", "MoonX (MOONX)",
#                                      "Matrix (MTRX)", "CryptoRulesEverythingAroundMe (CREAM)"),
#                         asa_id = c(432975976, 478549868, 384303832, 361806984, 388502764, 404719435, 234994096, 312412702),
#                         decimal = c(3,0, 0, 0, 6,5, 0, 6))
# 
# asa_index <- data.frame(asa_name = c("Commie Coin (USSR)",
#                                      "BirdBot (BIRDS)",
#                                      "AlgoMeow (MEOW)",
#                                      "Svansy Coin (SVANSY)", "MoonX (MOONX)",
#                                      "Matrix (MTRX)",
#                                      "CryptoRulesEverythingAroundMe (CREAM)"),
#                         asa_id = c(432975976, 478549868, 361806984, 388502764, 404719435,
#                                    234994096, 312412702),
#                         decimal = c(3, 0, 0, 6,5, 0, 6), 
#                         min_holding = c(200000, 10000, 100000, 10000000, 100000, 10000000, 10000))

# asa_index <- data.frame(asa_name = c("Commie Coin (USSR)",
#                                      "BirdBot (BIRDS)",
#                                      "AlgoMeow (MEOW)",
#                                      "CryptoRulesEverythingAroundMe (CREAM)",
#                                      "Parsec (PRSC)",
#                                      "Parsec AU (PRSCau)", 
#                                      "LOUDefi (LOUD)"),
#                         asa_id = c(432975976, 478549868, 361806984, 312412702,
#                                    415045633, 546713076, 457819394),
#                         decimal = c(3, 0, 0, 6, 0, 6, 6),
#                         min_holding = c(200000, 10000, 10000, 10000, 10000, 10000, 10000),
#                         minimum_tx = c(0,0,0,0,0,0,0))
pick_chain("algo")
asa_index <- data.frame(asa_name = c( "LOUDefi (LOUD)"),
                        asa_id = c(457819394),
                        decimal = c(6),
                        min_holding = c(10000),
                        minimum_tx = c(0))
update_networks(asa_index = asa_index, ncores = 6)
