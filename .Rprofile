# Use Posit Package Manager
options(
  renv.config.pak.enabled = TRUE,
  renv.config.ppm.enabled = TRUE,
  renv.config.ppm.default = TRUE
)

# Use renv
source("renv/activate.R")

# Set error handler to rlang
globalCallingHandlers(error = rlang::entrace)
