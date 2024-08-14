# Use Posit Package Manager
options(
  repos = c(CRAN = "https://p3m.dev/cran/latest"),
  renv.config.pak.enabled = TRUE
)

# Use renv
source("renv/activate.R")

# Set error handler to rlang
globalCallingHandlers(error = rlang::entrace)
