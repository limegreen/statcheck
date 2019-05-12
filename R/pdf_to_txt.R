# Inner function to read pdf:
pdf_to_txt <- function(x)
{
  txtfiles <- character(length(x))
  for (i in 1:length(x))
  {
    system(paste('pdftotext -q -enc "ASCII7" "', x[i], '"', sep = ""))
    if (file.exists(gsub("\\.pdf$", "\\.txt", x[i]))) {
      fileName <- gsub("\\.pdf$", "\\.txt", x[i])
      txtfiles[i] <- readChar(fileName, file.info(fileName)$size)
    } else{
      warning(paste("Failure in file", x[i]))
      txtfiles[i] <- ""
    }
  }
  return(txtfiles)
}

