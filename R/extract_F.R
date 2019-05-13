extract_F <- function(txt, OneTailedInTxt) {
  
  # Get location of F-values in text:
  # also pick up degrees of freedom wrongly converted into letters:
  # 1 --> l or I
  FLoc <-
    gregexpr(
      "F\\s?\\(\\s?\\d*\\.?(I|l|\\d+)\\s?,\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]n\\.?s\\.?)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
      txt,
      ignore.case = TRUE
    )[[1]]
  
  if (FLoc[1] != -1) {
    # Get raw text of F-values:
    FRaw <-
      substring(txt, FLoc, FLoc + attr(FLoc, "match.length") - 1)
    
    # convert wrongly printed "l" or "I" into 1
    FRaw <- gsub("l|I", 1, FRaw)
    
    # Extract location of numbers:
    nums <-
      gregexpr("(\\d*\\.?\\d+\\s?e?-?\\d*)|n\\.?s\\.?", FRaw, ignore.case = TRUE)
    
    # Extract df1:
    df1 <-
      as.numeric(substring(
        FRaw,
        sapply(nums, '[', 1),
        sapply(nums, function(x)
          x[1] + attr(x, "match.length")[1] - 1)
      ))
    
    # Extract df2:
    df2 <-
      as.numeric(substring(
        FRaw,
        sapply(nums, '[', 2),
        sapply(nums, function(x)
          x[2] + attr(x, "match.length")[2] - 1)
      ))
    
    # remove commas (thousands separators)
    Fsplit <- strsplit(FRaw, "\\)", perl = TRUE)
    
    FValsRaw <- lapply(Fsplit, function(x)
      x[2])
    FandDF <- lapply(Fsplit, function(x)
      x[1])
    
    FValsRaw <-
      gsub("(?<=\\d),(?=\\d+)", "", FValsRaw, perl = TRUE)
    
    FRaw <- paste(FandDF, ")", FValsRaw, sep = "")
    
    # Extract F-values
    numsF <- gregexpr("(\\d*\\.?\\d+)|n\\.?s\\.?", FValsRaw)
    suppressWarnings(FValsChar <-
                       substring(
                         FValsRaw,
                         sapply(numsF, '[', 1),
                         sapply(numsF, function(x)
                           x[1] + attr(x, "match.length")[1] - 1)
                       ))
    
    suppressWarnings(FVals <- as.numeric(FValsChar))
    
    # Extract number of decimals test statistic
    testdec <-
      attr(regexpr("\\.\\d+", FValsChar), "match.length") - 1
    testdec[testdec < 0] <- 0
    
    # Extract (in)equality test statistic
    testEqLoc <- gregexpr("\\)\\s?[<>=]", FRaw)
    testEq <- substring(
      FRaw,
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    
    # Extract p-values
    suppressWarnings(pValsChar <-
                       substring(
                         FValsRaw,
                         sapply(numsF, '[', 2),
                         sapply(numsF, function(x)
                           x[2] + attr(x, "match.length")[2] - 1)
                       ))
    
    suppressWarnings(pVals <- as.numeric(pValsChar))
    
    # Extract (in)equality
    eqLoc <-
      gregexpr("p\\s?[<>=]", FRaw, ignore.case = TRUE)
    pEq <- substring(
      FRaw,
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    pEq[grepl("n\\.?s\\.?", FRaw, ignore.case = TRUE)] <- "ns"
    
    # determine number of decimals of p value
    dec <-
      attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
    dec[dec < 0] <- NA
    
    # Create data frame:
    FRes <- data.frame(
      Source = names(txt),
      Statistic = "F",
      df1 = df1,
      df2 = df2,
      Test.Comparison = testEq,
      Value = FVals,
      Reported.Comparison = pEq,
      Reported.P.Value = pVals,
      Computed = pf(FVals, df1, df2, lower.tail = FALSE),
      Location = FLoc,
      Raw = FRaw,
      stringsAsFactors = FALSE,
      dec = dec,
      testdec = testdec,
      OneTailedInTxt = OneTailedInTxt
    )
    
    return(FRes)
  }
}