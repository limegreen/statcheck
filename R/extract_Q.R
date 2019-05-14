extract_Q <- function(txt, OneTailedInTxt) {
  # Get location of Q-values in text:
  QLoc <-
    gregexpr(
      "Q\\s?-?\\s?(w|within|b|between)?\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]n\\.?s\\.?)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
      txt,
      ignore.case = TRUE
    )[[1]]
  
  if (QLoc[1] != -1) {
    # Get raw text of t-values:
    QRaw <-
      substring(txt, QLoc, QLoc + attr(QLoc, "match.length") - 1)
    
    # remove commas (thousands separators)
    QRaw <- gsub("(?<=\\d),(?=\\d+)", "", QRaw, perl = TRUE)
    
    # Replace weird codings of a minus sign with actual minus sign:
    # First remove spaces
    QRaw <-
      gsub("(?<=\\=)\\s+(?=.*\\,)", "", QRaw, perl = TRUE)
    
    # Replace any weird string with a minus sign
    QRaw <-
      gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", " -", QRaw, perl = TRUE)
    
    # Add spaces again:
    QRaw <-
      gsub("(?<=\\=)(?=(\\.|\\d))", " ", QRaw, perl = TRUE)
    
    # Extract type of Q-test (general, within, or between)
    QtypeLoc <-
      gregexpr("Q\\s?-?\\s?(w|within|b|between)?",
               QRaw,
               ignore.case = TRUE)
    QtypeRaw <-
      substring(QRaw,
                sapply(QtypeLoc, '[', 1),
                sapply(QtypeLoc, function(x)
                  x[1] + attr(x, "match.length")[1] - 1))
    
    Qtype <- rep(NA, length(QtypeRaw))
    
    Qtype[grepl("Q\\s?-?\\s?(w|within)", QtypeRaw, ignore.case = TRUE)] <-
      "Qw"
    Qtype[grepl("Q\\s?-?\\s?(b|between)", QtypeRaw, ignore.case = TRUE)] <-
      "Qb"
    Qtype[is.na(Qtype)] <- "Q"
    
    # Extract location of numbers:
    nums <-
      gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|n\\.?s\\.?",
               QRaw,
               ignore.case = TRUE)
    
    # Extract df:
    df <-
      as.numeric(substring(
        QRaw,
        sapply(nums, '[', 1),
        sapply(nums, function(x)
          x[1] + attr(x, "match.length")[1] - 1)
      ))
    
    # Extract Q-values
    suppressWarnings(QValsChar <-
                       substring(
                         QRaw,
                         sapply(nums, '[', 2),
                         sapply(nums, function(x)
                           x[2] + attr(x, "match.length")[2] - 1)
                       ))
    
    suppressWarnings(QVals <- as.numeric(QValsChar))
    
    # Extract number of decimals test statistic
    testdec <-
      attr(regexpr("\\.\\d+", QValsChar), "match.length") - 1
    testdec[testdec < 0] <- 0
    
    # Extract (in)equality test statistic
    testEqLoc <- gregexpr("\\)\\s?[<>=]", QRaw)
    testEq <- substring(
      QRaw,
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1),
      sapply(testEqLoc, function(x)
        x[1] + attr(x, "match.length")[1] - 1)
    )
    
    # Extract p-values
    suppressWarnings(pValsChar <-
                       substring(
                         QRaw,
                         sapply(nums, '[', 3),
                         sapply(nums, function(x)
                           x[3] + attr(x, "match.length")[3] - 1)
                       ))
    
    suppressWarnings(pVals <- as.numeric(pValsChar))
    
    # Extract (in)equality
    eqLoc <-
      gregexpr("p\\s?[<>=]", QRaw, ignore.case = TRUE)
    pEq <- substring(QRaw,
                     sapply(eqLoc, function(x)
                       x[1] + attr(x, "match.length")[1] - 1),
                     sapply(eqLoc, function(x)
                       x[1] + attr(x, "match.length")[1] - 1))
    pEq[grepl("n\\.?s\\.?", QRaw, ignore.case = TRUE)] <- "ns"
    
    # determine number of decimals of p value
    dec <-
      attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
    dec[dec < 0] <- 0
    
    # Create data frame:
    QRes <- data.frame(
      Source = names(txt),
      Statistic = Qtype,
      df1 = NA,
      df2 = df,
      Test.Comparison = testEq,
      Value = QVals,
      Reported.Comparison = pEq,
      Reported.P.Value = pVals,
      Computed = pchisq(QVals, df, lower.tail =
                          FALSE),
      Location = QLoc,
      Raw = QRaw,
      stringsAsFactors = FALSE,
      dec = dec,
      testdec = testdec,
      OneTailedInTxt = OneTailedInTxt
    )
  }
}