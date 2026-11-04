# Lanceur ponctuel : source un script d'etape et appelle sa fonction d'entree
# avec la racine projet (meme logique que 00_run_pipeline_fixed.R, mode "function").
args <- commandArgs(trailingOnly = TRUE)
script <- args[1]; fn <- args[2]
PROJECT_DIR <- "D:/PREIS_AI_CLEAN"
env <- new.env()
sys.source(file.path(PROJECT_DIR, "R", "AI", script), envir = env)
stopifnot(exists(fn, envir = env, inherits = FALSE))
do.call(get(fn, envir = env), list(PROJECT_DIR))
cat("STAGE_OK", script, "\n")
