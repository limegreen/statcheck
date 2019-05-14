extract_chi2 <- function(txt, OneTailedInTxt) {
  # Get location of chi values or delta G in text:
  chi2Loc <-
    gregexpr(
      "((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trFzQWBnD ]\\s?)|([^trFzQWBnD ]2\\s?))2?\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)\\s?[<>=]\\s?\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]n\\.?s\\.?)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
      txt,
      ignore.case = TRUE
    )[[1]]
  
  if (chi2Loc[1] != -1) {
    # Get raw text of chi2-values:
    chi2Raw <-
      substring(txt, chi2Loc, chi2Loc + attr(chi2Loc, "match.length") - 1)
    substr(chi2Raw, 1, 1)[grepl("\\d", substr(chi2Raw, 1, 1))] <-
      " "
    
    # remove sample size if reported for calculations
    # save full result for "Raw" in final data frame
    chi2Raw_inclN <- chi2Raw
    chi2Raw <-
      gsub("N\\s?=\\s?\\d*\\,?\\d*\\,?\\d*",
           "",
           chi2Raw,
           ignore.case = TRUE)
    
    # remove commas (thousands separators)
    chi2Raw <-
      gsub("(?<=\\d),(?=\\d+\\.)", "", chi2Raw, perl = TRUE)
    
    # bug fix: remove extra opening brackets
    # if a chi2 result is reported between brackets, and the chi is not read by statcheck
    # the opening bracket is translated as the chi symbol, and extracting the numerics goes wrong
    chi2Raw <-
      gsub("\\((?=2\\s?\\()", "", chi2Raw, perl = TRUE)
    
    # Extract location of numbers:
    nums <-
      gregexpr(
        "(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|n\\.?s\\.?",
        sub("^.*?\\(", "", chi2Raw),
        ignore.case = TRUE
      )
    
    # Extract df:
    df <-
      as.numeric(substring(
        sub("^.*?\\(", "", chi2Raw),
        sapply(nums, '[', 1),
        sapply(nums, function(x)
          x[1] + attr(x, "match.length")[1] - 1)
      ))
    
    # Extract chi2-values
    suppressWarnings(chi2ValsChar <-
                       substring(
                         sub("^.*?\\(", "", chi2Raw),
                         sapply(nums, '[', 2),
                         sapply(nums, function(x)
                           x[2] + attr(x, "match.length")[2] - 1)
                       ))
    
    suppressWarnings(chi2Vals <- as.numeric(chi2ValsChar))
    
    # Extract number of decimals test statistic
    testdec <-
      attr(regexpr("\\.\\d+", chi2ValsChar), "match.length") - 1
    testdec[testdec < 0] <- 0
    
    # Extract (in)equality test statistic
    testEqLoc <- gregexpr("\\)\\s?[<>=]", chi2Raw)
    testEq <- substring(
      chi2Raw,
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    
    # Extract p-values
    suppressWarnings(pValsChar <-
                       substring(
                         sub("^.*?\\(", "", chi2Raw),
                         sapply(nums, '[', 3),
                         sapply(nums, function(x)
                           x[3] + attr(x, "match.length")[3] - 1)
                       ))
    
    suppressWarnings(pVals <- as.numeric(pValsChar))
    
    # Extract (in)equality
    eqLoc <-
      gregexpr("p\\s?[<>=]", chi2Raw, ignore.case = TRUE)
    pEq <- substring(
      chi2Raw,
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(eqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    pEq[grepl("n\\.?s\\.?", chi2Raw, ignore.case = TRUE)] <- "ns"
    
    # determine number of decimals of p value
    dec <-
      attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
    dec[dec < 0] <- 0
    
    # Create data frame:
    chi2Res <- data.frame(
      Source = names(txt),
      Statistic = "Chi2",
      df1 = df,
      df2 = NA,
      Test.Comparison = testEq,
      Value = chi2Vals,
      Reported.Comparison = pEq,
      Reported.P.Value = pVals,
      Computed = pchisq(chi2Vals, df, lower.tail =
                          FALSE),
      Location = chi2Loc,
      Raw = chi2Raw_inclN,
      stringsAsFactors = FALSE,
      dec = dec,
      testdec = testdec,
      OneTailedInTxt = OneTailedInTxt
    )
  }
}