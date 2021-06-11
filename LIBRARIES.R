# LIBRARIES

paketi <- c(
  "readr",
  "ggplot2",
  "dplyr",
  "class",
  "MASS",
  "caret",
  "devtools",
  "ggplot2",
  "countreg",
  "forcats",
  "AER",
  "pscl",
  "insurancerating",
  "Metrics",
  "sjPlot",
  "countreg",
  "GGally",
  "knitr",
  "nonnest2",
  "pscl"
)

for (paket in paketi) {
  if (!is.element(paket, .packages(all.available = TRUE))) {
    install.packages(paket)
  } 
  library(paket, character.only = TRUE)
}
rm(paket, paketi)