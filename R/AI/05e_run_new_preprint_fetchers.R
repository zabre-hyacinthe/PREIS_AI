setwd("D:/PREIS_AI")

source("R/AI/05b_fetch_arxiv_ai.R")
source("R/AI/05c_fetch_medrxiv_ai.R")
source("R/AI/05d_fetch_biorxiv_ai.R")

project_root <- "D:/PREIS_AI"

try(run_fetch_arxiv_ai(project_root = project_root, max_pages = 10, page_size = 100), silent = FALSE)
try(run_fetch_medrxiv_ai(project_root = project_root, from_date = Sys.Date() - 30, to_date = Sys.Date(), max_pages = 20), silent = FALSE)
try(run_fetch_biorxiv_ai(project_root = project_root, from_date = Sys.Date() - 30, to_date = Sys.Date(), max_pages = 20), silent = FALSE)

message("[PREIS-AI] New preprint sources fetched successfully.")