statcheck <-
  function(x,
           stat = c("t", "F", "cor", "chisq", "Z", "Q"),
           OneTailedTests = FALSE,
           alpha = .05,
           pEqualAlphaSig = TRUE,
           pZeroError = TRUE,
           OneTailedTxt = FALSE,
           AllPValues = FALSE){
  
    # create empty dataframes to append the results that are extracted in each step of the loop
    pRes <- data.frame(NULL)
    Res <- data.frame(NULL)
    
    if (length(x) == 0){
      return(Res)
    }
    
    # if the input object doesn't have a name, number the items in the list
    if (is.null(names(x))){
      names(x) <-  1:length(x)
    }
    

    # Set-up progress bar -----------------------------------------------------

    message("Extracting statistics...")
    pb <- txtProgressBar(max = length(x), style = 3)
    

    # Start for loop ----------------------------------------------------------

    for (i in 1:length(x)) {
      txt <- x[i]
      
      # Extract p-values --------------------------------------------------------

      # extract all p values in order to calculate the ratio (statcheck results)/(total # of p values)
      
      pvalues <- extract_p(txt)
      
      # append and close
      if(!is.null(pvalues)){
        pRes <- rbind(pRes, pvalues)
      }
      
      rm(pvalues)
      

      # Search for one-sided tests ----------------------------------------------

      # search for "one-sided"/"one-tailed"/"directional" in full text to detect one-sided testing
      
      onesided <-
        gregexpr("one.?sided|one.?tailed|directional", txt, ignore.case = TRUE)[[1]]
      
      if (onesided[1] != -1) {
        onesided <- 1
      } else {
        onesided <- 0
      }
      
      OneTailedInTxt <- as.logical(onesided)
      
      # t-tests -----------------------------------------------------------------

      # Extract t-tests
      if ("t" %in% stat) {
        
        tRes <- extract_t(txt, OneTailedInTxt = OneTailedInTxt)
        
        # Append, clean and close:
        if(!is.null(tRes)){
          Res <- rbind(Res, tRes)
        }
      
        rm(tRes)
      }
      
      # F-tests -----------------------------------------------------------------

      # Extract F-values:
      if ("F" %in% stat) {
        
        FRes <- extract_F(txt, OneTailedInTxt = OneTailedInTxt)
        
          # Append, clean and close:
        if(!is.null(FRes)){
          Res <- rbind(Res, FRes)
        }
          
        rm(FRes)
        
      }
      
      # Correlations ------------------------------------------------------------

      # Extract correlations:
      if (any(c("r", "cor", "correlations") %in% stat)) {
        
        rRes <- extract_r(txt, OneTailedInTxt = OneTailedInTxt)
        
          # Append, clean and close:
        if(!is.null(rRes)){
          Res <- rbind(Res, rRes)
        }
          
        rm(rRes)
      }
      

      # Z-tests -----------------------------------------------------------------

      # Extract z-tests:
      if ("Z" %in% stat) {
        
        zRes <- extract_z(txt, OneTailedInTxt = OneTailedInTxt)
        
        # Append, clean and close:
        if(!is.null(zRes)){
          Res <- rbind(Res, zRes)
        }
      
        rm(zRes)
      }
      
      # Chi2-tests --------------------------------------------------------------

      # Chis2-values:
      if ("chisq" %in% stat) {
        
        chi2Res <- extract_chi2(txt, OneTailedInTxt = OneTailedInTxt)
          
          # Append, clean and close:
        if(!is.null(chi2Res)){
          Res <- rbind(Res, chi2Res)
        }
        
          rm(chi2Res)
      }
      
      # Q-tests -----------------------------------------------------------------

      # Extract Q-tests
      if ("Q" %in% stat) {
        
        QRes <- extract_Q(txt, OneTailedInTxt = OneTailedInTxt)
          
          # Append, clean and close:
        if(!is.null(QRes)){
          Res <- rbind(Res, QRes)
        }
        
        rm(QRes)
        
      }

      # Close loop & progress bar -----------------------------------------------

      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    # Clean "Res" dataframe ---------------------------------------------------

    # Source <- NULL
    Res <- plyr::ddply(Res, .(Source), function(x)
      x[order(x$Location), ])
    
    if (nrow(Res) > 0) {
      # remove p values greater than one
      Res <- Res[Res$Reported.P.Value <= 1 |
                   is.na(Res$Reported.P.Value), ]
    }
    

    # Treat as one-tailed tests? ----------------------------------------------

    if (nrow(Res) > 0) {
      # if indicated, count all tests as onesided
      if (OneTailedTests == TRUE) {
        Res$Computed <- Res$Computed / 2
      }
      

      # Check for inconsistencies -----------------------------------------------

      # check for errors
      Res$Error <- ErrorTest(Res, alpha = alpha)
      
      # check for decision errors
      Res$DecisionError <-  DecisionErrorTest(Res, alpha = alpha, pEqualAlphaSig = pEqualAlphaSig)
      

      # Check decision errors with different alphas -----------------------------

      # check if there would also be a decision error if alpha=.01 or .1
      DecisionErrorAlphas <- logical()
      alphas <- c(.01, .1)
      
      for (a in alphas) {
        DecisionErrorAlphas <-
          c(DecisionErrorAlphas, DecisionErrorTest(Res, alpha = a, pEqualAlphaSig = pEqualAlphaSig))
      }
      
      if (any(DecisionErrorAlphas[!is.na(DecisionErrorAlphas) &
                                  !is.nan(DecisionErrorAlphas)])) {
        message(
          "\n Check the significance level. \n \n Some of the p value incongruencies are decision errors if the significance level is .1 or .01 instead of the conventional .05. It is recommended to check the actual significance level in the paper or text. Check if the reported p values are a decision error at a different significance level by running statcheck again with 'alpha' set to .1 and/or .01. \n "
        )
      }
      
      # Check if results are correct if one-sided -------------------------------

      if (OneTailedTests == FALSE) {
        # check if there could be one-sided tests in the data set
        
        computed <- Res$Computed
        comparison <- Res$Reported.Comparison
        reported <- Res$Reported.P.Value
        raw <- Res$Raw
        onetail <- computed / 2
        
        OneTail <- ifelse(
          Res$Error == TRUE &
            (
              grepl("=", comparison) & round(reported, 2) == round(onetail, 2)
            )
          |
            (
              grepl("<", comparison) &
                onetail < reported & computed > reported
            ),
          TRUE,
          FALSE
        )
        Res$OneTail <- OneTail
        
        if (any(OneTail[!is.na(OneTail)] == TRUE &
                OneTailedTxt[!is.na(OneTailedTxt)] == FALSE)) {
          message(
            "\n Check for one tailed tests. \n \n Some of the p value incongruencies might in fact be one tailed tests. It is recommended to check this in the actual paper or text. Check if the p values would also be incongruent if the test is indeed one sided by running statcheck again with 'OneTailedTests' set to TRUE. To see which Sources probably contain a one tailed test, try unique(x$Source[x$OneTail]) (where x is the statcheck output). \n "
          )
        }
        
      }
      

      # Correct for one-sided tests ---------------------------------------------

      # count errors as correct if they'd be correct one-sided
      # and there was a mention of 'one-sided','one-tailed', or 'directional' in the text
      
      
      if (OneTailedTxt == TRUE) {
        Res1tailed <- Res
        Res1tailed$Computed <- Res1tailed$Computed / 2
        
        Res1tailed$Error <- ErrorTest(Res1tailed, alpha = alpha)
        Res1tailed$DecisionError <- DecisionErrorTest(Res1tailed, alpha = alpha, pEqualAlphaSig = pEqualAlphaSig)
        
        Res$Error[!((
          Res$Statistic == "F" |
            Res$Statistic == "Chi2" |
            Res$Statistic == "Q"
        ) &
          Res$df1 > 1) &
          Res$OneTailedInTxt == TRUE & Res1tailed$Error == FALSE] <- FALSE
        
        Res$DecisionError[!((
          Res$Statistic == "F" |
            Res$Statistic == "Chi2" |
            Res$Statistic == "Q"
        ) &
          Res$df1 > 1) &
          Res$OneTailedInTxt == TRUE &
          Res1tailed$DecisionError == FALSE] <- FALSE
        
        
      }
      

      # Correct rounding --------------------------------------------------------

      # "correct" rounding differences
      # e.g. t=2.3 could be 2.25 to 2.34999999... with its range of p values
      correct_round <- numeric()
      
      lower <- Res$Value - (.5 / 10 ^ Res$testdec)
      upper <- Res$Value + (.5 / 10 ^ Res$testdec)
      
      for (i in seq_len(nrow(Res))) {
        if (Res[i, ]$Statistic == "F") {
          upP <- pf(lower[i], Res[i, ]$df1, Res[i, ]$df2, lower.tail = FALSE)
          lowP  <-
            pf(upper[i], Res[i, ]$df1, Res[i, ]$df2, lower.tail = FALSE)
          
        } else if (Res[i, ]$Statistic == "t") {
          if (lower[i] < 0) {
            lowP <- pt(lower[i], Res[i, ]$df2) * 2
            upP  <- pt(upper[i], Res[i, ]$df2) * 2
          } else{
            upP <- pt(-1 * lower[i], Res[i, ]$df2) * 2
            lowP  <- pt(-1 * upper[i], Res[i, ]$df2) * 2
          }
          
        } else if (Res[i, ]$Statistic == "Chi2" |
                   Res[i, ]$Statistic == "Q" |
                   Res[i, ]$Statistic == "Qw" | 
                   Res[i, ]$Statistic == "Qb") {
          upP <- pchisq(lower[i], Res[i, ]$df1, lower.tail = FALSE)
          lowP  <- pchisq(upper[i], Res[i, ]$df1, lower.tail = FALSE)
          
        } else if (Res[i, ]$Statistic == "r") {
          if (lower[i] < 0) {
            lowP <- pmin(pt(r2t(lower[i], Res[i, ]$df2), Res[i, ]$df2) * 2, 1)
            upP  <-
              pmin(pt(r2t(upper[i], Res[i, ]$df2), Res[i, ]$df2) * 2, 1)
          } else {
            upP <- pmin(pt(-1 * r2t(lower[i], Res[i, ]$df2), Res[i, ]$df2) * 2, 1)
            lowP  <-
              pmin(pt(-1 * r2t(upper[i], Res[i, ]$df2), Res[i, ]$df2) * 2, 1)
          }
          
        } else if (Res[i, ]$Statistic == "Z" |
                   Res[i, ]$Statistic == "z") {
          if (lower[i] < 0) {
            lowP <- pnorm(abs(lower[i]), lower.tail = FALSE) * 2
            upP  <- pnorm(abs(upper[i]), lower.tail = FALSE) * 2
          } else {
            upP <- pnorm(lower[i], lower.tail = FALSE) * 2
            lowP  <- pnorm(upper[i], lower.tail = FALSE) * 2
          }
          
        }
        
        if (OneTailedTests == TRUE) {
          upP <- upP / 2
          lowP <- lowP / 2
        }
        
        if (Res[i, "Reported.Comparison"] == "=") {
          correct_round[i] <-
            ifelse(
              Res[i, ]$Error == TRUE &
                Res$Reported.P.Value[i] >= round(lowP, Res$dec[i]) &
                Res$Reported.P.Value[i] <= round(upP, Res$dec[i]),
              TRUE,
              FALSE
            )
        }
        
        if (Res[i, "Reported.Comparison"] == "<") {
          correct_round[i] <-
            ifelse(Res[i, ]$Error == TRUE &
                     Res$Reported.P.Value[i] > lowP, TRUE, FALSE)
        }
        
        if (Res[i, "Reported.Comparison"] == ">") {
          correct_round[i] <-
            ifelse(Res[i, ]$Error == TRUE &
                     Res$Reported.P.Value[i] < upP, TRUE, FALSE)
        }
        
        
      }
      
      CorrectRound <- as.logical(correct_round)
      
      Res$Error[CorrectRound] <- FALSE
      Res$DecisionError[CorrectRound] <- FALSE
      
      # Impossible p-values -----------------------------------------------------

      # p values smaller or equal to zero are errors
      
      if (pZeroError == TRUE) {
        ImpossibleP <- (Res$Reported.P.Value <= 0)
      } else {
        ImpossibleP <- (Res$Reported.P.Value < 0)
      }
      
      Res$Error[ImpossibleP] <- TRUE
      

      # Only DecisionError when Error -------------------------------------------

      # p values that are not an error can also not be a decision error
      # this happens sometimes when reported= "p=.05" and e.g. computed=.052...
      # this should be counted as correct
      
      NoErrorDecisionError <-
        Res$Error == FALSE & Res$DecisionError == TRUE
      Res$DecisionError[NoErrorDecisionError] <- FALSE
      

      # APAfactor ---------------------------------------------------------------
      
      # APAfactor: proportion of APA results (that statcheck reads) of total number of p values
      
      # select only the results of pRes that are from articles with at least 1 statcheck result
      pRes_selection <- pRes[pRes$Source %in% Res$Source, ]
      
      # select only the statcheck results that are from an article with at least one p value
      # this is relevant, because it sometimes happens that statcheck extracts less p values
      # p values than statcheck results. For instance in cases when a p value appears to be
      # greater than 1.
      
      Res_selection <-
        Res[Res$Source %in% pRes_selection$Source, ]
      APA <-
        by(Res_selection, Res_selection$Source, nrow) / by(pRes_selection, pRes_selection$Source, nrow)
      Res$APAfactor <-
        round(as.numeric(apply(Res, 1, function(x)
          APA[which(names(APA) == x["Source"])])), 2)
      

      # Final Res dataframe -----------------------------------------------------

      # final data frame
      Res <- data.frame(
        Source = Res$Source,
        Statistic = Res$Statistic,
        df1 = Res$df1,
        df2 = Res$df2,
        Test.Comparison = Res$Test.Comparison,
        Value = Res$Value,
        Reported.Comparison = Res$Reported.Comparison,
        Reported.P.Value = Res$Reported.P.Value,
        Computed = Res$Computed,
        Raw = Res$Raw,
        Error = Res$Error,
        DecisionError = Res$DecisionError,
        OneTail = Res$OneTail,
        OneTailedInTxt = Res$OneTailedInTxt,
        APAfactor = Res$APAfactor
      )
      
      class(Res) <- c("statcheck", "data.frame")
    }
    
    # Return ------------------------------------------------------------------

    if (AllPValues == FALSE) {
      # Return message when there are no results
      if (nrow(Res) > 0) {
        return(Res)
      } else {
        Res <- cat("statcheck did not find any results\n")
      }
      
    } else {
      return(pRes)
    }
  }