#' @description Install packages if it was not installed and load them.
#'
installNeedPacks <- function() {
  packages <- c("plyr", "RWeka", "rminer", "Hmisc", "RPushbullet")
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
pbSetup("o.87sSLXaQ9tglgRB3DUN0h79XcvvwdXCV")