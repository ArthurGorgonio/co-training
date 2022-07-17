#' @description Configure the experiment to run with a single call of the shell
#'   using some kind of parametrization to evaluate any config.
#'
atribArgs <- function(arguments, databases) {
  params <- list()
  params$ratios <- c(0.05, 0.1, 0.2)
  params$selection_rate <- c(0.05, 0.1, 0.15, 0.2)
  params$ebal_concordance <- c(0.6, 0.75)
  params$iniIndex <- 1
  params$finIndex <- length(databases)
  arg <- 1
  while (arg < length(arguments)) {
    param <- as.numeric(arguments[arg + 1])
    switch (arguments[arg],
            '-s' = {
              params$iniIndex <- param
            },
            '-e' = {
              params$finIndex <- param
            },
            '-l' = {
              cat(param)
              params$lengthBatch <- param
            },
            '-r' = {
              params$ratios <- param
            },
            '-d' = {
              params$seeds <- param
            },
            '-h' = {
              cat('-s to start data set [1-42]\n-e to end data set [1-42]',
                  '-l to length batch\n-r to ratio\n-d to seed!!!')
            }
    )
    arg <- arg + 2
  }
  return(params)
}


#' @description Install packages if it was not installed and load them.
#'
installNeedPacks <- function() {
  packages <- c("plyr", "RWeka", "rminer", "Hmisc")
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
