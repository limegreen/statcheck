extract_z <- function(txt, OneTailedInTxt) {
  # Get location of z-values in text:
  zLoc <-
    gregexpr(
      "[^a-z]z\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]n\\.?s\\.?)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
      txt,
      ignore.case = TRUE
    )[[1]]
  
  if (zLoc[1] != -1) {
    # Get raw text of z-values:
    zRaw <-
      substring(txt, zLoc, zLoc + attr(zLoc, "match.length") - 1)
    
    # remove any character before test statistic
    zRaw <- gsub(".?(z|Z)", "Z", zRaw, perl = TRUE)
    
    # remove commas (thousands separators)
    zRaw <-
      gsub("(?<=\\d),(?=\\d+\\.)", "", zRaw, perl = TRUE)
    
    # Replace weird codings of a minus sign with actual minus sign:
    # First remove spaces
    zRaw <-
      gsub("(?<=\\=)\\s+(?=.*\\,)", "", zRaw, perl = TRUE)
    
    # Replace any weird string with a minus sign
    zRaw <-
      gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", " -", zRaw, perl = TRUE)
    
    # Add spaces again:
    zRaw <-
      gsub("(?<=\\=)(?=(\\.|\\d))", " ", zRaw, perl = TRUE)
    
    # Extract location of numbers:
    nums <-
      gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|n\\.?s\\.?",
               zRaw,
               ignore.case = TRUE)
    
    # Extract z-values
    suppressWarnings(zValsChar <-
                       substring(
                         zRaw,
                         sapply(nums, '[', 1),
                         sapply(nums, function(x)
                           x[1] + attr(x, "match.length")[1] - 1)
                       ))
    
    suppressWarnings(zVals <- as.numeric(zValsChar))
    
    # Extract number of decimals test statistic
    testdec <-
      attr(regexpr("\\.\\d+", zValsChar), "match.length") - 1
    testdec[testdec < 0] <- 0
    
    # Extract (in)equality test statistic
    testEqLoc <- gregexpr("(z|Z|z'|Z')\\s?[<>=]", zRaw)
    testEq <- substring(
      zRaw,
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    
    # Extract p-values
    suppressWarnings(pValsChar <-
                       substring(
                         zRaw,
                         sapply(nums, '[', 2),
                         sapply(nums, function(x)
                           x[2] + attr(x, "match.length")[2] - 1)
                       ))
    
    suppressWarnings(pVals <- as.numeric(pValsChar))
    
    # Extract (in)equality
    eqLoc <-
      gregexpr("p\\s?[<>=]", zRaw, ignore.case = TRUE)
    pEq <- substring(
      zRaw,
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    pEq[grepl("n\\.?s\\.?", zRaw, ignore.case = TRUE)] <- "ns"
    
    # determine number of decimals of p value
    dec <-
      attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
    dec[dec < 0] <- 0
    
    # Create data frame:
    zRes <- data.frame(
      Source = names(txt),
      Statistic = "Z",
      df1 = NA,
      df2 = NA,
      Test.Comparison = testEq,
      Value = zVals,
      Reported.Comparison = pEq,
      Reported.P.Value = pVals,
      Computed = pnorm(abs(zVals), lower.tail = FALSE) *
        2,
      Location = zLoc,
      Raw = zRaw,
      stringsAsFactors = FALSE,
      dec = dec,
      testdec = testdec,
      OneTailedInTxt = OneTailedInTxt
    )
    
    return(zRes)
  }
}