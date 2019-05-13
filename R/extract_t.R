extract_t <- function(txt, OneTailedInTxt) {
  
  # Get location of t-values in text:
  tLoc <-
    gregexpr(
      "t\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]n\\.?s\\.?)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
      txt,
      ignore.case = TRUE
    )[[1]]
  
  if (tLoc[1] != -1) {
    # Get raw text of t-values:
    tRaw <-
      substring(txt, tLoc, tLoc + attr(tLoc, "match.length") - 1)
    
    # remove commas (thousands separators)
    tRaw <- gsub("(?<=\\d),(?=\\d+)", "", tRaw, perl = TRUE)
    
    # Replace weird codings of a minus sign with actual minus sign:
    # First remove spaces
    tRaw <-
      gsub("(?<=\\=)\\s+(?=.*\\,)", "", tRaw, perl = TRUE)
    
    # Replace any weird string with a minus sign
    tRaw <-
      gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", " -", tRaw, perl = TRUE)
    
    # Add spaces again:
    tRaw <-
      gsub("(?<=\\=)(?=(\\.|\\d))", " ", tRaw, perl = TRUE)
    
    # Extract location of numbers:
    nums <-
      gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|n\\.?s\\.?",
               tRaw,
               ignore.case = TRUE)
    
    # Extract df:
    df <-
      as.numeric(substring(
        tRaw,
        sapply(nums, '[', 1),
        sapply(nums, function(x)
          x[1] + attr(x, "match.length")[1] - 1)
      ))
    
    # Extract t-values
    suppressWarnings(tValsChar <-
                       substring(
                         tRaw,
                         sapply(nums, '[', 2),
                         sapply(nums, function(x)
                           x[2] + attr(x, "match.length")[2] - 1)
                       ))
    
    suppressWarnings(tVals <- as.numeric(tValsChar))
    
    # Extract number of decimals test statistic
    testdec <-
      attr(regexpr("\\.\\d+", tValsChar), "match.length") - 1
    testdec[testdec < 0] <- 0
    
    # Extract (in)equality test statistic
    testEqLoc <- gregexpr("\\)\\s?[<>=]", tRaw)
    testEq <- substring(
      tRaw,
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    
    # Extract p-values
    suppressWarnings(pValsChar <-
                       substring(
                         tRaw,
                         sapply(nums, '[', 3),
                         sapply(nums, function(x)
                           x[3] + attr(x, "match.length")[3] - 1)
                       ))
    
    suppressWarnings(pVals <- as.numeric(pValsChar))
    
    # Extract (in)equality
    eqLoc <-
      gregexpr("p\\s?[<>=]", tRaw, ignore.case = TRUE)
    pEq <- substring(
      tRaw,
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    pEq[grepl("n\\.?s\\.?", tRaw, ignore.case = TRUE)] <- "ns"
    
    # determine number of decimals of p value
    dec <-
      attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
    dec[dec < 0] <- 0
    
    # Create data frame:
    tRes <- data.frame(
      Source = names(txt),
      Statistic = "t",
      df1 = NA,
      df2 = df,
      Test.Comparison = testEq,
      Value = tVals,
      Reported.Comparison = pEq,
      Reported.P.Value = pVals,
      Computed = pt(-1 * abs(tVals), df) * 2,
      Location = tLoc,
      Raw = tRaw,
      stringsAsFactors = FALSE,
      dec = dec,
      testdec = testdec,
      OneTailedInTxt = OneTailedInTxt
    )
    
   return(tRes)
     
  }
}