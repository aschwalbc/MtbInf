rm(list=ls())

suppressPackageStartupMessages({
   library(here) 
   library(tidyverse)
   library(deSolve)
   library(data.table)
   library(fst)
})

# 1. Load data ==========
args <- commandArgs(trailingOnly = TRUE)
data_file_name <- args[1]

# 1. Load data ==========
ARI <- read.fst(here(data_file_name))

all_iso <- unique(as.character(ARI$iso3))
fileContent <- file("iso_list.txt")
writeLines(all_iso, fileContent)
close(fileContent)

all_reps <- unique(as.character(ARI$rep))
fileContent <- file("rep_list.txt")
writeLines(all_reps, fileContent)
close(fileContent)

list_of_dfs <- split(ARI, list(ARI$iso3, ARI$rep))

invisible(lapply(names(list_of_dfs), function(name) {
  file_path <- sprintf('ari/%s.fst', name)
  
  if (!file.exists(file_path)) {
    write.fst(list_of_dfs[[name]], file_path)
  }
}))
