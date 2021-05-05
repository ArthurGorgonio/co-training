#' @description Install packages if it was not installed and load them.
#'
installNeedPacks <- function() {
  packages <- c("SSLR", "plyr", "DMwR2", "ssc", "RWeka", "caret", "dplyr",
                "tidymodels", "rminer")
  if (!require("BiocManager")) {
    install.packages("BiocManager")
  }
  for (pack in packages) {
    if (!require(pack, character.only = TRUE)) {
      BiocManager::install(pack)
    }
    library(pack, character.only = TRUE, verbose = F)
  }
}

installNeedPacks()
tidymodels_prefer()