extract_r <- function(txt, OneTailedInTxt) {
  # Get location of r-values in text:
  rLoc <-
    gregexpr(
      "r\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*\\.?\\d+\\s?,\\s?(([^a-z]n\\.?s\\.?)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
      txt,
      ignore.case = TRUE
    )[[1]]
  
  if (rLoc[1] != -1) {
    # Get raw text of r-values:
    rRaw <-
      substring(txt, rLoc, rLoc + attr(rLoc, "match.length") - 1)
    
    # Replace weird codings of a minus sign with actual minus sign:
    # First remove spaces
    rRaw <-
      gsub("(?<=\\=)\\s+(?=.*\\,)", "", rRaw, perl = TRUE)
    
    # Replace any weird string with a minus sign
    rRaw <-
      gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", " -", rRaw, perl = TRUE)
    
    # Add spaces again:
    rRaw <-
      gsub("(?<=\\=)(?=(\\.|\\d))", " ", rRaw, perl = TRUE)
    
    # Extract location of numbers:
    nums <-
      gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|n\\.?s\\.?",
               rRaw,
               ignore.case = TRUE)
    
    # Extract df:
    df <-
      as.numeric(substring(
        rRaw,
        sapply(nums, '[', 1),
        sapply(nums, function(x)
          x[1] + attr(x, "match.length")[1] - 1)
      ))
    
    # Extract r-values
    suppressWarnings(rValsChar <-
                       substring(
                         rRaw,
                         sapply(nums, '[', 2),
                         sapply(nums, function(x)
                           x[2] + attr(x, "match.length")[2] - 1)
                       ))
    
    suppressWarnings(rVals <- as.numeric(rValsChar))
    
    # Extract number of decimals test statistic
    testdec <-
      attr(regexpr("\\.\\d+", rValsChar), "match.length") - 1
    testdec[testdec < 0] <- 0
    
    
    # Extract (in)equality test statistic
    testEqLoc <- gregexpr("\\)\\s?[<>=]", rRaw)
    testEq <- substring(
      rRaw,
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    
    # Extract p-values
    suppressWarnings(pValsChar <-
                       substring(
                         rRaw,
                         sapply(nums, '[', 3),
                         sapply(nums, function(x)
                           x[3] + attr(x, "match.length")[3] - 1)
                       ))
    
    suppressWarnings(pVals <- as.numeric(pValsChar))
    
    # Extract (in)equality
    eqLoc <-
      gregexpr("p\\s?[<>=]", rRaw, ignore.case = TRUE)
    pEq <- substring(
      rRaw,
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    pEq[grepl("n\\.?s\\.?", rRaw, ignore.case = TRUE)] <- "ns"
    
    
    # determine number of decimals of p value
    dec <-
      attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
    dec[dec < 0] <- 0
    
    # computed p = NA for correlations reported as >1
    pComputed <-
      pmin(pt(-1 * abs(r2t(rVals, df)), df) * 2, 1)
    pComputed[is.nan(pComputed)] <- NA
    
    # Create data frame:
    rRes <- data.frame(
      Source = names(txt),
      Statistic = "r",
      df1 = NA,
      df2 = df,
      Test.Comparison = testEq,
      Value = rVals,
      Reported.Comparison = pEq,
      Reported.P.Value = pVals,
      Computed = pComputed,
      Location = rLoc,
      Raw = rRaw,
      stringsAsFactors = FALSE,
      dec = dec,
      testdec = testdec,
      OneTailedInTxt = OneTailedInTxt
    )
    
    return(rRes)
  }
}