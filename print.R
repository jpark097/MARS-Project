#Print.R File

print.mars <- function(input) {
  NameColumn <- colnames(input$B)
  A <- length(NameColumn)
  cat("Coefficients:", "\n")
  for (i in 1:A) {
    cat(paste0(NameColumn[i], ": ", signif(coefficients(input)[i]), digits = 5), " ")
  }
}

