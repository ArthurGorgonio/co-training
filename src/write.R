#' @description Write in the output file the content.
#'
#' @usage write_archive (title, prefix, content, dataName, acc, f1, preci,
#'   recall, begin, end, samplesPerIt, append = T, row = F, col = F, sep = " ")
#'
#' @param title The title of the file.
#' @param prefix The directore where the file be storage.
#' @param dataName The name of the current database.
#' @param append The method to write in the archive.
#' @param acc A vector with all folds accuracies.
#' @param f1 A vectorr with all folds F-measure.
#' @param preci A vector with all folds precision measure.
#' @param recall A vector with all folds recall.
#' @param begin Time when start the database processing.
#' @param end Time when end the database processing.
#' @param samplesPerIt A vector with the number of samples to be added in each 
#'   iteration of the method.
#' @param append An optional parameter to append the current content in file.
#'   (Default TRUE).
#' @param row An optional parameter to write rows in the file. (Default FALSE).
#' @param col An optional parameter to write cols in the file. (Default FALSE).
#' @param sep An optional parameter to use in paste. (Default " " Single space).
#'
writeArchive <- function(title, prefix, dataName, method, acc, f1, preci,
                         recall, begin, end, append = T, row = F, col = F,
                         sep = " ") {
  acc <- round(acc, 4)
  f1 <- round(f1, 4)
  preci <- round(preci, 4)
  recall <- round(recall, 4)
  pattern <- "%d/%m/%Y %X"
  filePath <- paste(prefix, title, sep = "/")
  separ <- paste(rep("-", 80), collapse = "")
  metrics <- "\taccura\terror\tfmeasu\tprecis\trecall"
  dbName <- paste("@DATASET:", dataName)
  folds <- "@Folds\t: 10"
  version <- paste("@SSL approach\t:", method, sep = "")
  headers <- paste(separ, dbName, folds, version, separ, metrics,
                   separ, sep = "\n")
  line <- c()
  for (i in 1:10) {
    line <- paste(line,
                  paste("fold", i, ":\t", acc[i], " ", round(1 - acc[i], 4),
                        " ", f1[i], " ", preci[i], " ", recall[i], sep = ""),
                  sep = "\n")
  }
  allMeans <- paste("AVERAG\t", round(mean(acc), 4), " ",
                    round(mean(round(1 - acc, 4)), 4), " ",
                    round(mean(f1), 4), " ", round(mean(preci), 4), " ",
                    round(mean(recall), 4), sep = "")
  line <- paste(line, separ, allMeans, separ, sep = "\n")
  time <- paste("BEGIN:\t", format(begin, pattern), "\nEND:\t", 
                format(end, pattern), "\n\nTIME ELAPSED: ",
                round(as.numeric(difftime(end, begin, units = "secs")), 3),
                "\n", separ, sep = "")
  content <- paste(headers, line, time, sep = "\n")
  write(content, filePath, append = append, sep = sep)
}
