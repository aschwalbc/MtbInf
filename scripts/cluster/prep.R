rm(list=ls())

suppressPackageStartupMessages({
   library(here)
   library(fst)
})

# Gets command line arguments
args <- commandArgs(trailingOnly = TRUE)
data <- args[1]

# Load data
ARI <- read.fst(here(data))

# Creates .txt file with all unique ISO3 codes
all_iso <- unique(as.character(ARI$iso3))
fileContent <- file("iso_list.txt")
writeLines(all_iso, fileContent)
close(fileContent)

# Creates .txt file with all repetitions (1-200)
all_reps <- unique(as.character(ARI$rep))
fileContent <- file("rep_list.txt")
writeLines(all_reps, fileContent)
close(fileContent)

# Split data frame into ISO3 and repetitions 
isorep <- split(ARI, list(ARI$iso3, ARI$rep))

# Creates .fst files for each ISO3 and repetitions
invisible(lapply(names(isorep), function(name) {
  file_path <- sprintf('ari/%s.fst', name)
  write.fst(isorep[[name]], file_path)
}))
