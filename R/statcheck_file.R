statcheck_file <- function(fileName, ...){
  
  if (missing(fileName))
      fileName <- tcltk::tk_choose.files()
    
    txts <- file_to_txt(fileName)
    
    if(!is.null(txts)){
      
    names(txts) <-
      gsub("\\.pdf$ | \\.html?$", "", basename(fileName), perl = TRUE)
    
    return(statcheck(txts, ...))
    
    } else {
      
      cat("Please provide either a PDF or HTML file. No other file types are currently supported.")
    }
}