library(curl)
library(jsonlite)
library(magrittr)
library(dplyr)
library(janitor)
library(tidyr)
library(igraph)
library(lubridate)
library(foreach)

pick_chain <- function(chain = "algo") {
  if(chain == "algo") {
    source("utils/algo_utils.R")
  } else if (chain == "ethereum") {
    source("utils/eth_utils.R")
  }
}