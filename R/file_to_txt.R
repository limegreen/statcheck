file_to_txt <- function(file){
  
  if(grepl("\\.(pdf$)|(html?$)", file, ignore.case = TRUE)){
    
    ### PDF TO TEXT
    if(grepl("\\.pdf$", file, ignore.case = TRUE)){
      
      # create first raw text file
      strings <- pdftools::pdf_text(file)
      
      # create 1 character vector per input file by pasting the results of different pages together
      strings <- paste(strings, collapse = " ")
      
      # encode everything in UTF-32 (to have same output accross multiple operating systems)
      strings <- stringi::stri_enc_toutf32(strings) 
      
      # Replace known weird characters ------------------------------------
      
      # substitute double solidous (UTF-32 Decimal 11005) with equal sign (UTF-32 Decimal 61) 
      # [issue in JPSP, JEP, APA journals]
      strings <- gsub(pattern = "11005", replacement = "61", strings, fixed = TRUE)
      
      # substitute U+2B0D (C++ \u2b0d; UTF-32 Decimal 11021) with equal less than sign (UTF-32 Decimal 60)
      # [issue in JPSP, JEP, APA journals]
      strings <- gsub(pattern = "11021", replacement = "60", strings, fixed = TRUE) 
      
      # substitute U+2AFA (UTF-32 Decimal 11002) with HYPHEN-MINUS sign (UTF-32 Decimal 45) 
      # [issue in JPSP, APA journals]
      strings <- gsub(pattern = "11002", replacement = "45", strings, fixed = TRUE) 
      
      # substitute U+2439 (C++ \u2439; UTF-32 Decimal 9273) with small greek chi (UTF-32 Decimal 967) #
      # [issue in APA journals]
      strings <- gsub(pattern = "9273", replacement = "967", strings, fixed = TRUE) 
      
      # Revert to UTF-8 --------------------------------------------------
      strings <- stringi::stri_enc_fromutf32(strings)
      
      # Arrange text according to paper column layout
      # strings <- pdf_columns(strings) # function from pdf_process.R script
      
    }
    
    ### HTML TO TEXT
    
    else if(grepl("\\.html?$", file, ignore.case = TRUE)) {
      
      # open connection to file
      con <- file(file)
      
      # convert file to raw plain text 
      strings <- readChar(con, file.info(file)$size, useBytes = TRUE)
      
      # Remove subscripts (except for p_rep)
      strings <- gsub(pattern = "<sub>(?!rep).*?</sub>", replacement = "", x = strings, perl = TRUE)
      
      # Remove HTML tags:
      strings <- gsub(pattern = "<(.|\n)*?>", replacement = "", strings)
      
      # Replace html codes:
      strings <- gsub(pattern = "&#60;", replacement = "<", strings, fixed = TRUE)
      strings <- gsub(pattern = "&lt;", replacement = "<", strings, fixed = TRUE)
      strings <- gsub(pattern = "&#61;", replacement = "=", strings, fixed = TRUE)
      strings <- gsub(pattern = "&#62;", replacement = ">", strings, fixed = TRUE)
      strings <- gsub(pattern = "&gt;", replacement = ">", strings, fixed = TRUE)
      strings <- gsub(pattern = "&#40;", replacement = "(", strings, fixed = TRUE)
      strings <- gsub(pattern = "&#41;", replacement = ")", strings, fixed = TRUE)
      strings <- gsub(pattern = "&thinsp;", replacement = " ", strings, fixed = TRUE)
      strings <- gsub(pattern = "&nbsp;", replacement = " ", strings, fixed = TRUE)
      strings <- gsub(pattern = "\n", replacement = "", strings)
      strings <- gsub(pattern = "\r", replacement = "", strings)
      strings <- gsub(pattern = "\\s+", replacement = " ", strings)
      strings <- gsub(pattern = "&minus;", replacement = "-", strings, fixed = TRUE)
    
      # close the connection to the file again
      close(con)
        
    }
    
    return(strings)
    
  } 
  
}

