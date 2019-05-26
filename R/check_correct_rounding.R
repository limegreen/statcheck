check_correct_rounding <- function(Res){
  correct_round <- numeric()
  
  lower <- Res$Value - (.5 / 10 ^ Res$testdec)
  upper <- Res$Value + (.5 / 10 ^ Res$testdec)
  
  for (i in seq_len(nrow(Res))) {
    if (Res[i, ]$Statistic == "F") {
      upP <- pf(lower[i], Res[i, ]$df1, Res[i, ]$df2, lower.tail = FALSE)
      lowP <- pf(upper[i], Res[i, ]$df1, Res[i, ]$df2, lower.tail = FALSE)
      
    } else if (Res[i, ]$Statistic == "t") {
      if (lower[i] < 0) {
        lowP <- pt(lower[i], Res[i, ]$df2) * 2
        upP  <- pt(upper[i], Res[i, ]$df2) * 2
      } else{
        upP <- pt(-1 * lower[i], Res[i, ]$df2) * 2
        lowP  <- pt(-1 * upper[i], Res[i, ]$df2) * 2
      }
      
    } else if (Res[i, ]$Statistic %in% c("Chi2", "Q", "Qw", "Qb")) {
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
      
    } else if (Res[i, ]$Statistic %in% c("Z", "z")) {
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
  
  return(as.logical(correct_round))
}