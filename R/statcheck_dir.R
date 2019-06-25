statcheck_dir <- function(dir, 
                          subdir = FALSE, 
                          pdf_html = c("both", "pdf", "html"),
                          ...){
 
  # give pop-up window if file path is not provided
  if(missing(dir)) {
    dir <- tcltk::tk_choose.dir()
  }
  
  # depending on setting, search for pdf and/or html files in the directory
  pdf_html <- match.arg(pdf_html)
  
  if(pdf_html == "both"){
    extension = "\\.(html?$|pdf$)"
  } else if(pdf_html == "pdf"){
    extension = "\\.pdf$"
  } else {
    extension = "\\.html?$"
  }
  
  # select the files that statcheck needs to check -------------
 
   files <- list.files(dir,
                      pattern = extension,
                      full.names = TRUE,
                      recursive = subdir)
  
  if (length(files) == 0) {
    stop("No PDF or HTML file found")
  }
  
  # convert files to plain text --------------------------------
  
  txts <- character(length(files))
  
  message("Importing PDF/HTML files...")
  pb <- txtProgressBar(max = length(files), style = 3)
  
  for (i in 1:length(files)) {
    txts[i] <-  file_to_txt(files[i])
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  names(txts) <- gsub("\\.(html?$|pdf$)", "", basename(files))
  return(statcheck(txts, ...))
  
}
