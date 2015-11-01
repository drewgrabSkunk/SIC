#ADX and TR Formulations#

tech.transform.fun.2 <- function(URL, ticker) {
  output.file <-
    paste(
      "C:/Users/faskham/Desktop/Files/",ticker,".csv", sep =
        ""
    )
  if (file.exists(output.file)) {
    file.remove(output.file)
  }
  
  tech <- read.csv(file = URL)
  tech$Date <- as.POSIXct(tech$Date)
  tech <- tech[order(tech$Date),]
  tech$ticker <- ticker
  
  #Next 1, 7, 30 Day Gain#
  tech$next.gain <- NA
  for (i in 1:(length(tech$Close) - 1)) {
    tech$next.gain[i] <- tech$Close[i + 1] - tech$Open[i+1]
    tech$next.gain.per[i] <- tech$next.gain[i] / tech$Open[i+1]
  }
  
  
  tech$next.gain.7 <- NA
  for (i in 1:(length(tech$Close) - 6)) {
    tech$next.gain.7[i] <- tech$Close[i + 7] - tech$Open[i+1]
    tech$next.gain.7.per[i] <- tech$next.gain.7[i] / tech$Open[i+1]
  }
  
  
  tech$next.gain.30 <- NA
  for (i in 1:(length(tech$Close) - 29)) {
    tech$next.gain.30[i] <- tech$Close[i + 30] - tech$Open[i+1]
    tech$next.gain.30.per[i] <- tech$next.gain.30[i] / tech$Open[i+1]
  }
  
  
  #Calculate High-Low, High-prevClose, and Low-prevClose#
  tech$hilo <- tech$High - tech$Low
  tech$loclose[1] <- NA
  tech$hiclose[1] <- NA
  for (i in 2:length(tech$Close)) {
    tech$loclose[i] <- abs(tech$Low[i] - tech$Close[i - 1])
  }
  for (i in 2:length(tech$Close)) {
    tech$hiclose[i] <- abs(tech$High[i] - tech$Close[i - 1])
  }
  
  #TR Calculation: Max of three calculations above#
  tech$tr[1] <- NA
  for (i in 2:length(tech$Close)) {
    tech$tr[i] <-
      max(tech$hilo[i], tech$hiclose[i], tech$loclose[i], na.rm = TRUE)
  }
  
  #ATR - 10 day moving average of TR#
  tech$atr[1] <- NA
  tech$atr[2:10] <- NA
  for (i in 11:length(tech$Close)) {
    tech$atr[i] <- mean(tech$tr[(i - 9):i])
  }
  
  #+DM 1 Calculation#
  tech$hidiff.1[1] <- NA
  tech$lodiff.1[1] <- NA
  tech$p.DM.1[1] <- NA
  
  for (i in 2:length(tech$Close)) {
    tech$hidiff.1[i] <- tech$High[i] - tech$High[i - 1]
  }
  for (i in 2:length(tech$Close)) {
    tech$lodiff.1[i] <- tech$Low[i - 1] - tech$Low[i]
  }
  
  for (i in 2:length(tech$Close)) {
    if (tech$hidiff.1[i] > tech$lodiff.1[i]) {
      tech$p.DM.1[i] <- max(tech$hidiff.1[i],0)
    } else {
      tech$p.DM.1[i] <- 0
    }
  }
  
  #-DM 1 Calculation#
  tech$n.DM.1[1] <- NA
  for (i in 2:length(tech$Close)) {
    if (tech$lodiff.1[i] > tech$hidiff.1[i]) {
      tech$n.DM.1[i] <- max(tech$lodiff.1[i],0)
    } else {
      tech$n.DM.1[i] <- 0
    }
  }
  
  #TR14 Calculation#
  tech$tr.14[1] <- NA
  tech$tr.14[1:14] <- c(rep(NA,14))
  tech$tr.14[15] <- sum(tech$tr[2:15],na.rm = TRUE)
  for (i in 16:length(tech$Close)) {
    tech$tr.14[i] <-
      (tech$tr.14[i - 1] - (tech$tr.14[i - 1] / 14) + tech$tr[i])
  }
  
  #+DM 14 Calculation#
  tech$p.DM.14[1] <- NA
  tech$p.DM.14[1:14] <- c(rep(NA,14))
  tech$p.DM.14[15] <- sum(tech$p.DM.1[2:15],na.rm = TRUE)
  for (i in 16:length(tech$Close)) {
    tech$p.DM.14[i] <-
      (tech$p.DM.14[i - 1] - (tech$p.DM.14[i - 1] / 14) + tech$p.DM.1[i])
  }
  
  #-DM 14 Calculation#
  tech$n.DM.14[1] <- NA
  tech$n.DM.14[1:14] <- c(rep(NA,14))
  tech$n.DM.14[15] <- sum(tech$n.DM.1[2:15],na.rm = TRUE)
  for (i in 16:length(tech$Close)) {
    tech$n.DM.14[i] <-
      (tech$n.DM.14[i - 1] - (tech$n.DM.14[i - 1] / 14) + tech$n.DM.1[i])
  }
  
  #+DI 14 Calculation#
  tech$p.DI.14[1] <- NA
  tech$p.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$p.DI.14[i] <- (100 * (tech$p.DM.14[i] / tech$tr.14[i]))
  }
  
  #-DI 14 Calculation#
  tech$n.DI.14[1] <- NA
  tech$n.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$n.DI.14[i] <- (100 * (tech$n.DM.14[i] / tech$tr.14[i]))
  }
  
  #DI 14 Diff Calculation#
  tech$diff.DI.14[1] <- NA
  tech$diff.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$diff.DI.14[i] <- abs(tech$p.DI.14[i] - tech$n.DI.14[i])
  }
  
  #DI 14 Sum Calculation#
  tech$sum.DI.14[1] <- NA
  tech$sum.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$sum.DI.14[i] <- tech$p.DI.14[i] + tech$n.DI.14[i]
  }
  
  #DX Calculation#
  tech$DX[1] <- NA
  tech$DX[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$DX[i] <- (100 * (tech$diff.DI.14[i] / tech$sum.DI.14[i]))
  }
  
  #ADX Calculation#
  tech$ADX[1] <- NA
  tech$ADX[1:27] <- c(rep(NA,27))
  tech$ADX[28] <- mean(tech$DX[15:28])
  for (i in 29:length(tech$Close)) {
    tech$ADX[i] <- (((tech$ADX[i - 1] * 13) + tech$DX[i]) / 14)
  }
  
  #20 Day Moving Average#
  require(zoo)
  
  tech$MA.20[1] <- NA
  tech$MA.20[1:19] <- c(rep(NA,19))
  tech$MA.20[20:length(tech$Close)] <-
    rollmean(tech$Close, 20)
  
  #20 Day Moving STD#
  tech$STD.20[1] <- NA
  tech$STD.20[1:19] <- c(rep(NA,19))
  for (i in 20:length(tech$Close)) {
    tech$STD.20[i] <- sd(tech$Close[(i - 19):i]) * (sqrt(19 / 20))
  }
  
  #Bollinger Bands(20,2)#
  tech$up.Bol <- tech$MA.20 + (2 * tech$STD.20)
  tech$down.Bol <- tech$MA.20 - (2 * tech$STD.20)
  tech$Bollinger <- tech$up.Bol - tech$down.Bol
  
  #A-D Line#
  tech$MF.Mult <-
    ((tech$Close - tech$Low) - (tech$High - tech$Close)) / (tech$High - tech$Low)
  
  tech$MF.Value <- tech$MF.Mult * tech$Volume
  
  tech$ADLine[1] <- tech$MF.Value[1]
  for (i in 20:length(tech$Close)) {
    tech$ADLine[i] <- (tech$ADLine[i - 1] + tech$MF.Value[i])
  }
  
  #CCI 20-Day#
  require(lsr)
  
  tech$Typical.Price <-
    (tech$High + tech$Low + tech$Close) / 3
  
  tech$SMA.20[1] <- NA
  tech$SMA.20[1:19] <- c(rep(NA,19))
  tech$SMA.20[20:length(tech$Close)] <-
    rollmean(tech$Typical.Price, 20)
  
  tech$MAD.20[1] <- NA
  tech$MAD.20[1:19] <- c(rep(NA,19))
  for (i in 20:length(tech$Close)) {
    tech$MAD.20[i] <- aad(tech$Typical.Price[(i - 19):i])
  }
  
  tech$CCI.20 <-
    (tech$Typical.Price - tech$SMA.20) / (.015 * tech$MAD.20)
  
  #CMF 20-Day#
  tech$CMF.20[1] <- NA
  tech$CMF.20[1:19] <- c(rep(NA,19))
  tech$CMF.20[20:length(tech$Close)] <-
    rollmean(tech$MF.Value, 20) / rollmean(tech$Volume, 20)
  
  #OBV Calculation#
  tech$OBV[1] <- 0
  for (i in 2:length(tech$Close)) {
    if (tech$Close[i] > tech$Close[i - 1]) {
      tech$OBV[i] <- (tech$OBV[i - 1] + tech$Volume[i])
    }
    if (tech$Close[i] < tech$Close[i - 1]) {
      tech$OBV[i] <- (tech$OBV[i - 1] - tech$Volume[i])
    }
    else {
      tech$OBV[i] <- tech$OBV[i - 1]
    }
  }
  
  #Money Flow Index#
  tech$updown[1] <- NA
  for (i in 2:length(tech$Close)) {
    if (tech$Typical.Price[i] > tech$Typical.Price[i - 1]) {
      tech$updown[i] <- 1
    }
    else {
      tech$updown[i] <- -1
    }
  }
  
  tech$Raw.Flow <- tech$Typical.Price * tech$Volume
  
  tech$Pos.Flow.1[1] <- NA
  for (i in 2:length(tech$Close)) {
    if (tech$updown[i] > 0) {
      tech$Pos.Flow.1[i] <- tech$Raw.Flow[i]
    }
    else {
      tech$Pos.Flow.1[i] <- 0
    }
  }
  
  tech$Neg.Flow.1[1] <- NA
  for (i in 2:length(tech$Close)) {
    if (tech$updown[i] < 0) {
      tech$Neg.Flow.1[i] <- tech$Raw.Flow[i]
    }
    else {
      tech$Neg.Flow.1[i] <- 0
    }
  }
  
  tech$Pos.Flow.14[1] <- NA
  tech$Pos.Flow.14[2:14] <- NA
  for (i in 15:length(tech$Close)) {
    tech$Pos.Flow.14[i] <- sum(tech$Pos.Flow.1[(i - 13):i])
  }
  
  tech$Neg.Flow.14[1] <- NA
  tech$Neg.Flow.14[2:14] <- NA
  for (i in 15:length(tech$Close)) {
    tech$Neg.Flow.14[i] <- sum(tech$Neg.Flow.1[(i - 13):i])
  }
  
  tech$Ratio.Flow.14 <- tech$Pos.Flow.14 / tech$Neg.Flow.14
  
  tech$Index.Flow.14 <- (100 - (100 / (1 + tech$Ratio.Flow.14)))
  
  #Stochastic Oscillator 14-Day#
  tech$hi.14[1] <- NA
  tech$hi.14[2:13] <- NA
  for (i in 14:length(tech$Close)) {
    tech$hi.14[i] <- max(tech$High[(i - 13):i])
  }
  
  tech$lo.14[1] <- NA
  tech$lo.14[2:13] <- NA
  for (i in 14:length(tech$Close)) {
    tech$lo.14[i] <- min(tech$Low[(i - 13):i])
  }
  
  tech$lo.14[1] <- NA
  tech$lo.14[2:13] <- NA
  tech$Osc.14 <-
    ((tech$Close - tech$lo.14) / (tech$hi.14 - tech$lo.14)) * 100
  
  #Williams %R 14-Day#
  tech$Will.14 <-
    ((tech$hi.14 - tech$Close) / (tech$hi.14 - tech$lo.14)) * (-100)
  
  #EMA 12, 20, 26 Day#
  tech$EMA.9[1] <- NA
  tech$EMA.9[2:8] <- NA
  tech$EMA.9[9] <- mean(tech$Close[1:9])
  for (i in 10:length(tech$Close)) {
    tech$EMA.9[i] <-
      (((2 / 10) * (tech$Close[i] - tech$EMA.9[i - 1])) + tech$EMA.9[(i-1)])
  }
  
  tech$EMA.12[1] <- NA
  tech$EMA.12[2:11] <- NA
  tech$EMA.12[12] <- mean(tech$Close[1:12])
  for (i in 13:length(tech$Close)) {
    tech$EMA.12[i] <-
      (((2 / 13) * (tech$Close[i] - tech$EMA.12[i - 1])) + tech$EMA.12[(i-1)])
  }
  
  tech$EMA.20[1] <- NA
  tech$EMA.20[2:19] <- NA
  tech$EMA.20[20] <- mean(tech$Close[1:20])
  for (i in 21:length(tech$Close)) {
    tech$EMA.20[i] <-
      (((2 / 21) * (tech$Close[i] - tech$EMA.20[i - 1])) + tech$EMA.20[(i-1)])
  }
  
  tech$EMA.26[1] <- NA
  tech$EMA.26[2:25] <- NA
  tech$EMA.26[26] <- mean(tech$Close[1:26])
  for (i in 27:length(tech$Close)) {
    tech$EMA.26[i] <-
      (((2 / 27) * (tech$Close[i] - tech$EMA.26[i - 1])) + tech$EMA.26[(i-1)])
  }
  
  #KC Bands#
  tech$kc.upper <- (tech$EMA.20 + (2 * tech$atr))
  tech$kc.lower <- (tech$EMA.20 - (2 * tech$atr))
  tech$kc.width <- tech$kc.upper - tech$kc.lower
  
  #Bollinger/Keltner Comparison#
  tech$bb.kc[1] <- NA
  tech$bb.kc[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$up.Bol[i] < tech$kc.upper[i]) &
        (tech$down.Bol[i] > tech$kc.lower[i])) {
      tech$bb.kc[i] <- 1
    }
    else {
      tech$bb.kc[i] <- 0
    }
  }
  
  
  #CCI Cross 100 Up#
  tech$CCI.cross.up[1] <- NA
  tech$CCI.cross.up[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] > 100) &
        (tech$CCI.20[i - 1] < 100)) {
      tech$CCI.cross.up[i] <- 1
    }
    else {
      tech$CCI.cross.up[i] <- 0
    }
  }
  
  #CCI Cross neg 100 Up#
  tech$CCI.cross.neg.up[1] <- NA
  tech$CCI.cross.neg.up[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] > -100) &
        (tech$CCI.20[i - 1] < -100)) {
      tech$CCI.cross.neg.up[i] <- 1
    }
    else {
      tech$CCI.cross.neg.up[i] <- 0
    }
  }
  
  #CCI Cross 100 Down#
  tech$CCI.cross.down[1] <- NA
  tech$CCI.cross.down[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] < 100) &
        (tech$CCI.20[i - 1] > 100)) {
      tech$CCI.cross.down[i] <- 1
    }
    else {
      tech$CCI.cross.down[i] <- 0
    }
  }
  
  #CCI Cross 0 Down#
  tech$CCI.cross.0.down[1] <- NA
  tech$CCI.cross.0.down[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] < 0) &
        (tech$CCI.20[i - 1] > 0)) {
      tech$CCI.cross.0.down[i] <- 1
    }
    else {
      tech$CCI.cross.0.down[i] <- 0
    }
  }
  
  #CCI Cross negative 100 Down#
  tech$CCI.cross.neg.100.down[1] <- NA
  tech$CCI.cross.neg.100.down[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] < -100) &
        (tech$CCI.20[i - 1] > -100)) {
      tech$CCI.cross.neg.100.down[i] <- 1
    }
    else {
      tech$CCI.cross.neg.100.down[i] <- 0
    }
  }
  
  #CCI Above#
  tech$CCI.above[1] <- NA
  tech$CCI.above[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if (tech$CCI.20[i] > 100) {
      tech$CCI.above[i] <- 1
    }
    else {
      tech$CCI.above[i] <- 0
    }
  }
  
  
  #DI Cross#
  tech$DI.cross[1] <- NA
  tech$DI.cross[2:15] <- NA
  for (i in 16:length(tech$Close)) {
    if ((tech$p.DI.14[i] > tech$n.DI.14[i]) &
        (tech$p.DI.14[(i - 1)] < tech$n.DI.14[(i - 1)])) {
      tech$DI.cross[i] <- 1
    }
    else {
      tech$DI.cross[i] <- 0
    }
  }
  
  #DI Cross down#
  tech$DI.cross.down[1] <- NA
  tech$DI.cross.down[2:15] <- NA
  for (i in 16:length(tech$Close)) {
    if ((tech$p.DI.14[i] < tech$n.DI.14[i]) &
        (tech$p.DI.14[(i - 1)] > tech$n.DI.14[(i - 1)])) {
      tech$DI.cross.down[i] <- 1
    }
    else {
      tech$DI.cross.down[i] <- 0
    }
  }
  
  #DI Above#
  tech$DI.above[1] <- NA
  tech$DI.above[2:15] <- NA
  for (i in 16:length(tech$Close)) {
    if (tech$p.DI.14[i] > tech$n.DI.14[i]) {
      tech$DI.above[i] <- 1
    }
    else {
      tech$DI.above[i] <- 0
    }
  }
  
  
  #MACD 14 Calculation#
  tech$MACD[1] <- NA
  tech$MACD[2:25] <- NA
  for (i in 26:length(tech$Close)) {tech$MACD[i] <- tech$EMA.12[i] - tech$EMA.26[i]}
  
  
  #MACD 17 Calculation#
  tech$MACD.17[1] <- NA
  tech$MACD.17[2:25] <- NA
  for (i in 26:length(tech$Close)) {tech$MACD.17[i] <- tech$EMA.9[i] - tech$EMA.26[i]}
  
  #MACD 14 Signal#
  tech$MACD.signal[1] <- NA
  tech$MACD.signal[2:33] <- NA
  tech$MACD.signal[34] <- mean(tech$MACD[26:34])
  for (i in 35:length(tech$Close)) {
    tech$MACD.signal[i] <-
      (((2 / 27) * (tech$MACD[i] - tech$MACD.signal[i - 1])) + tech$MACD.signal[(i-1)])
  }
  
  #MACD 17 Signal#
  tech$MACD.17.signal[1] <- NA
  tech$MACD.17.signal[2:33] <- NA
  tech$MACD.17.signal[34] <- mean(tech$MACD.17[26:34])
  for (i in 35:length(tech$Close)) {
    tech$MACD.17.signal[i] <-
      (((2 / 27) * (tech$MACD.17[i] - tech$MACD.17.signal[i - 1])) + tech$MACD.17.signal[(i-1)])
  }
  
  #MA 14 Cross Down#
  tech$MA.cross.down[1] <- NA
  tech$MA.cross.down[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if ((tech$MACD[i] > tech$MACD.signal[i]) &
        (tech$MACD[(i - 1)] < tech$MACD.signal[(i - 1)])) {
      tech$MA.cross.down[i] <- 1
    }
    else {
      tech$MA.cross.down[i] <- 0
    }
  }
  
  #MA 14 Cross Up#
  tech$MA.cross.up[1] <- NA
  tech$MA.cross.up[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if ((tech$MACD[i] < tech$MACD.signal[i]) &
        (tech$MACD[(i - 1)] > tech$MACD.signal[(i - 1)])) {
      tech$MA.cross.up[i] <- 1
    }
    else {
      tech$MA.cross.up[i] <- 0
    }
  }
  
  #MA 17 Cross Down#
  tech$MA.17.cross.down[1] <- NA
  tech$MA.17.cross.down[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if ((tech$MACD.17[i] > tech$MACD.17.signal[i]) &
        (tech$MACD.17[(i - 1)] < tech$MACD.17.signal[(i - 1)])) {
      tech$MA.17.cross.down[i] <- 1
    }
    else {
      tech$MA.17.cross.down[i] <- 0
    }
  }
  
  #MA 17 Cross Up#
  tech$MA.17.cross.up[1] <- NA
  tech$MA.17.cross.up[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if ((tech$MACD.17[i] < tech$MACD.17.signal[i]) &
        (tech$MACD.17[(i - 1)] > tech$MACD.17.signal[(i - 1)])) {
      tech$MA.17.cross.up[i] <- 1
    }
    else {
      tech$MA.17.cross.up[i] <- 0
    }
  }
  
  #MA 14 Above#
  tech$MA.above[1] <- NA
  tech$MA.above[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if (tech$MACD[i] > tech$MACD.signal[i]) {
      tech$MA.above[i] <- 1
    }
    else {
      tech$MA.above[i] <- 0
    }
  }
  
  #MA 14 Below#
  tech$MA.below[1] <- NA
  tech$MA.below[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if (tech$MACD[i] < tech$MACD.signal[i]) {
      tech$MA.below[i] <- 1
    }
    else {
      tech$MA.below[i] <- 0
    }
  }
  
  #CCI, DI, MA Cross 3#
  tech$CCI.cross.3[1] <- NA
  tech$CCI.cross.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$CCI.cross.3[i] <- tech$CCI.cross.up[i] + tech$CCI.cross.up[i-1] + tech$CCI.cross.up[i-2]}
  
  tech$DI.cross.3[1] <- NA
  tech$DI.cross.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$DI.cross.3[i] <- tech$DI.cross[i] + tech$DI.cross[i-1] + tech$DI.cross[i-2]}
  
  tech$DI.cross.down.3[1] <- NA
  tech$DI.cross.down.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$DI.cross.down.3[i] <- tech$DI.cross.down[i] + tech$DI.cross.down[i-1] + tech$DI.cross.down[i-2]}
  
  tech$MA.cross.3[1] <- NA
  tech$MA.cross.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$MA.cross.3[i] <- tech$MA.cross.up[i] + tech$MA.cross.up[i-1] + tech$MA.cross.up[i-2]}
  
  tech$CCI.cross.neg.3[1] <- NA
  tech$CCI.cross.neg.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$CCI.cross.neg.3[i] <- tech$CCI.cross.neg.100.down[i] + tech$CCI.cross.neg.100.down[i-1] + tech$CCI.cross.neg.100.down[i-2]}
  
  #Price KC#
  tech$price.kc[1] <- NA
  tech$price.kc[2:19] <- NA
  for (i in 20:length(tech$Close)) {
    if (tech$Close[i] > tech$kc.upper[i]) {tech$price.kc[i] <- 1} 
    else {tech$price.kc[i] <- 0}
  }
  
  #Price KC Short#
  tech$price.kc.short[1] <- NA
  tech$price.kc.short[2:19] <- NA
  for (i in 20:length(tech$Close)) {
    if (tech$Close[i] < tech$kc.lower[i]) {tech$price.kc.short[i] <- 1} 
    else {tech$price.kc.short[i] <- 0}
  }
  
  #Buy Triple Cross 3#
  tech$buy.three.cross[1] <- NA
  tech$buy.three.cross[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$bb.kc[i] == 1) & (tech$CCI.cross.3[i] == 1) & (tech$DI.cross.3[i] == 1) & (tech$MA.cross.3[i] == 1)) {
      tech$buy.three.cross[i] <- 1}
    else { tech$buy.three.cross[i] <- 0}
  }
  
  #Buy Roll DI Cross 3 (DI Roll = 6 Days)#
  tech$buy.roll.DI[1] <- NA
  tech$buy.roll.DI[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$bb.kc[i] == 1) & (tech$CCI.cross.3[i] == 1) & (tech$MA.cross.3[i] == 1) & (tech$DI.cross.3[i] == 1)
       & (tech$DI.cross.3[i-1] == 1) & (tech$DI.cross.3[i-2] == 1) & (tech$DI.cross.3[i-3] == 1)
       & (tech$DI.cross.3[i-4] == 1) & (tech$DI.cross.3[i-5] == 1)) {
      tech$buy.roll.DI[i] <- 1}
    else { tech$buy.roll.DI[i] <- 0}
  }
  
  #Buy MA Above#
  tech$buy.MA.above[1] <- NA
  tech$buy.MA.above[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$price.kc[i] == 1) & (tech$CCI.cross.3[i] == 1) & (tech$MA.above[i] == 1) & (tech$DI.cross.3[i] == 1)) {
      tech$buy.MA.above[i] <- 1}
    else { tech$buy.MA.above[i] <- 0}
  }
  
  #Short MA Above inverse#
  tech$short.MA.inverse[1] <- NA
  tech$short.MA.inverse[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$price.kc.short[i] == 1) & (tech$CCI.cross.neg.3[i] == 1) & (tech$MA.below[i] == 1) & (tech$DI.cross.down.3[i] == 1)) {
      tech$short.MA.inverse[i] <- 1}
    else { tech$short.MA.inverse[i] <- 0}
  }
  
  
  #Buy Triple Above#
  tech$buy.triple.above[1] <- NA
  tech$buy.triple.above[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$bb.kc[i] == 1) & (tech$CCI.above[i] == 1) & (tech$MA.above[i] == 1) & (tech$DI.above[i] == 1)) {
      tech$buy.triple.above[i] <- 1}
    else { tech$buy.triple.above[i] <- 0}
  }
  
  #Buy Triple Above Modified#
  tech$buy.triple.mod[1] <- NA
  tech$buy.triple.mod[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if((tech$buy.triple.above[i] == 1) & (tech$CCI.above[i-1] == 1) & (tech$CCI.above[i-2] == 1) & (tech$CCI.above[i-3] == 1)) {
      tech$buy.triple.mod[i] <- 1}
    else { tech$buy.triple.mod[i] <- 0}
  }
  
  #Sell on Cross Down#
  tech$sell.cross.down[1] <- NA
  tech$sell.cross.down[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if(((tech$MA.17.cross.down[i] == 1) || (tech$MA.17.cross.down[i-1] == 1) || (tech$MA.17.cross.down[i-2] == 1) || (tech$MA.17.cross.down[i-3] == 1) || (tech$MA.17.cross.down[i-4] == 1))
       & ((tech$bb.kc[i] == 1 || tech$bb.kc[i-1] == 1 || tech$bb.kc[i-2] == 1 || tech$bb.kc[i-3] == 1 || tech$bb.kc[i-4] == 1))
       & ( (tech$CCI.cross.down[i] == 1) ||  (tech$CCI.cross.down[i-1] == 1)   ||  (tech$CCI.cross.down[i-2] == 1) ||  (tech$CCI.cross.down[i-3] == 1) ||  (tech$CCI.cross.down[i-4] == 1)
           ||  (tech$CCI.cross.0.down[i] == 1) || (tech$CCI.cross.0.down[i-1] == 1) || (tech$CCI.cross.0.down[i-2] == 1) || (tech$CCI.cross.0.down[i-3] == 1) || (tech$CCI.cross.0.down[i-4] == 1))) {
      tech$sell.cross.down[i] <- 1}
    else { tech$sell.cross.down[i] <- 0}
  }
  
  
  #Cover on Cross Up#
  tech$cover.cross.up[1] <- NA
  tech$cover.cross.up[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if(((tech$MA.17.cross.up[i] == 1) || (tech$MA.17.cross.up[i-1] == 1) || (tech$MA.17.cross.up[i-2] == 1) || (tech$MA.17.cross.up[i-3] == 1) || (tech$MA.17.cross.up[i-4] == 1))
       & ((tech$bb.kc[i] == 1 || tech$bb.kc[i-1] == 1 || tech$bb.kc[i-2] == 1 || tech$bb.kc[i-3] == 1 || tech$bb.kc[i-4] == 1))
       & ( (tech$CCI.cross.neg.up[i] == 1) ||  (tech$CCI.cross.neg.up[i-1] == 1)   ||  (tech$CCI.cross.neg.up[i-2] == 1) ||  (tech$CCI.cross.neg.up[i-3] == 1) ||  (tech$CCI.cross.neg.up[i-4] == 1)
           ||  (tech$CCI.cross.neg.up[i-5] == 1) || (tech$CCI.cross.neg.up[i-6] == 1) || (tech$CCI.cross.neg.up[i-7] == 1) || (tech$CCI.cross.neg.up[i-8] == 1) || (tech$CCI.cross.neg.up[i-9] == 1))) {
      tech$cover.cross.up[i] <- 1}
    else { tech$cover.cross.up[i] <- 0}
  }
  
  #Independent Sell Signals#
  tech$sell.MA.cross.down[1] <- NA
  tech$sell.MA.cross.down[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if((tech$MA.17.cross.down[i] == 1) || (tech$MA.17.cross.down[i-1] == 1) || (tech$MA.17.cross.down[i-2] == 1) || (tech$MA.17.cross.down[i-3] == 1) || (tech$MA.17.cross.down[i-4] == 1)) {
      tech$sell.MA.cross.down[i] <- 1}
    else { tech$sell.MA.cross.down[i] <- 0}
  }
  
  tech$sell.bb.kc[1] <- NA
  tech$sell.bb.kc[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if((tech$bb.kc[i] == 1 || tech$bb.kc[i-1] == 1 || tech$bb.kc[i-2] == 1 || tech$bb.kc[i-3] == 1 || tech$bb.kc[i-4] == 1)) {
      tech$sell.bb.kc[i] <- 1}
    else { tech$sell.bb.kc[i] <- 0}
  }
  
  tech$sell.CCI.cross.down[1] <- NA
  tech$sell.CCI.cross.down[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if((tech$CCI.cross.down[i] == 1) ||  (tech$CCI.cross.down[i-1] == 1)   ||  (tech$CCI.cross.down[i-2] == 1) ||  (tech$CCI.cross.down[i-3] == 1) ||  (tech$CCI.cross.down[i-4] == 1)
       ||  (tech$CCI.cross.0.down[i] == 1) || (tech$CCI.cross.0.down[i-1] == 1) || (tech$CCI.cross.0.down[i-2] == 1) || (tech$CCI.cross.0.down[i-3] == 1) || (tech$CCI.cross.0.down[i-4] == 1)) {
      tech$sell.CCI.cross.down[i] <- 1}
    else { tech$sell.CCI.cross.down[i] <- 0}
  }
  
  #Holding Gain on Next Sell Cross Down#
  tech$holding.gain.sell.1[1] <- NA
  tech$holding.gain.sell.1[2:39] <- NA
  tech$holding.gain.sell.per[1] <- NA
  tech$holding.gain.sell.per[2:39] <- NA
  tech$holding.latency[1] <- NA
  tech$holding.latency[2:39] <- NA
  for( i in 40:length(tech$Close)) {
    if((tech$buy.three.cross[i] == 0) & (tech$buy.roll.DI[i] == 0) & (tech$buy.MA.above[i] == 0) & (tech$buy.triple.above[i] == 0) & (tech$buy.triple.mod[i] == 0)) {
      tech$holding.gain.sell.1[i] <- 0
    }
    
    else if(1 %in% tech$sell.cross.down[i:length(tech$Close)]) {
      tech$holding.gain.sell.1[i] <- tech$Open[(min(which(tech$sell.cross.down[i:length(tech$Close)] == 1)) + i)] - tech$Open[i+1]
      tech$holding.gain.sell.per[i] <- tech$holding.gain.sell.1[i]/tech$Open[i+1]
      tech$holding.latency[i] <- (min(which(tech$sell.cross.down[i:length(tech$Close)] == 1)))+1
    }
    
    else {tech$holding.gain.sell.1[i] <- NA}
  }
  
  #Holding Gain on Next Sell Cross Down#
  tech$holding.gain.short[1] <- NA
  tech$holding.gain.short[2:39] <- NA
  tech$holding.gain.short.per[1] <- NA
  tech$holding.gain.short.per[2:39] <- NA
  tech$holding.latency.short[1] <- NA
  tech$holding.latency.short[2:39] <- NA
  for( i in 40:length(tech$Close)) {
    if((tech$short.MA.inverse[i] == 0)) {
      tech$holding.gain.sell.1[i] <- 0
    }
    
    else if(1 %in% tech$cover.cross.up[i:length(tech$Close)]) {
      tech$holding.gain.short[i] <- (tech$Open[(min(which(tech$cover.cross.up[i:length(tech$Close)] == 1)) + i)] - tech$Open[i+1])*(-1)
      tech$holding.gain.short.per[i] <- tech$holding.gain.short[i]/tech$Open[i+1]
      tech$holding.latency.short[i] <- (min(which(tech$cover.cross.up[i:length(tech$Close)] == 1)))+1
    }
    
    else {tech$holding.gain.short[i] <- NA}
  }
  
  write.csv(tech, output.file, row.names = FALSE)
  
  return(tech)
  
}
