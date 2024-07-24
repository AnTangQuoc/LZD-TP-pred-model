renv::settings$snapshot.type("implicit")
renv::dependencies()
renv::snapshot()

# Load necessary package
library(jsonlite)

# Read the renv.lock file
lockfile <- fromJSON("renv.lock")

# Extract package names and versions
packages <- lockfile$Packages
package_names <- names(packages)
package_versions <- sapply(packages, function(pkg) pkg$Version)

# Create DESCRIPTION content
description_content <- c(
  "Type: project",
  "Title: LZD-TP-pred-model",
  "Description: LZD-TP-pred-model",
  "Depends:",
  paste0("    ", paste(package_names, package_versions, sep = " (>= ", collapse = "),\n    "), ")"),
  "License: CC-BY",
  "Encoding: UTF-8",
  "LazyData: true"
)

# Write to DESCRIPTION file
writeLines(description_content, "DESCRIPTION")

renv::settings$snapshot.type("explicit")
