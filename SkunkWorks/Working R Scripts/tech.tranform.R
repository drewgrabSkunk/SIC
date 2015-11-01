#ADX and TR Formulations#

tech.transform.fun <- function(URL, ticker) {
  output.file <-
    paste(
      "//FREDCO/SkunkWorks/Ticker.Output/",ticker,".csv", sep =
        ""
    )
  if (file.exists(output.file)) {
    file.remove(output.file)
  }
  
  adx.tr <- read.csv(file = URL)
  adx.tr$Date <- as.POSIXct(adx.tr$Date)
  adx.tr <- adx.tr[order(adx.tr$Date),]
  
  #Next 1, 7, 30 Day Gain#
  adx.tr$next.gain <- NA
  for (i in 1:(length(adx.tr$Close) - 1)) {
    adx.tr$next.gain[i] <- adx.tr$Close[i + 1] - adx.tr$Close[i]
  }
  adx.tr$next.gain.per <- adx.tr$next.gain / adx.tr$Close
  
 # adx.tr$next.gain <- NA
 # for (i in 1:(length(adx.tr$Close) - 1)) {
 #   adx.tr$next.gain[i] <- adx.tr$Close[i + 1] - adx.tr$Close[i]
 # }
 # adx.tr$next.gain.per <- adx.tr$next.gain / adx.tr$Close
  
  #adx.tr$next.gain <- NA
 # for (i in 1:(length(adx.tr$Close) - 1)) {
 #   adx.tr$next.gain[i] <- adx.tr$Close[i + 1] - adx.tr$Close[i]
 # }
 # adx.tr$next.gain.per <- adx.tr$next.gain / adx.tr$Close
  
  #Calculate High-Low, High-prevClose, and Low-prevClose#
  adx.tr$hilo <- adx.tr$High - adx.tr$Low
  adx.tr$loclose[1] <- NA
  adx.tr$hiclose[1] <- NA
  for (i in 2:length(adx.tr$Close)) {
    adx.tr$loclose[i] <- abs(adx.tr$Low[i] - adx.tr$Close[i - 1])
  }
  for (i in 2:length(adx.tr$Close)) {
    adx.tr$hiclose[i] <- abs(adx.tr$High[i] - adx.tr$Close[i - 1])
  }
  
  #TR Calculation: Max of three calculations above#
  adx.tr$tr[1] <- NA
  for (i in 2:length(adx.tr$Close)) {
    adx.tr$tr[i] <-
      max(adx.tr$hilo[i], adx.tr$hiclose[i], adx.tr$loclose[i], na.rm = TRUE)
  }
  
  #ATR - 10 day moving average of TR#
  adx.tr$atr[1] <- NA
  adx.tr$atr[2:10] <- NA
  for (i in 11:length(adx.tr$Close)) {
    adx.tr$atr[i] <- mean(adx.tr$tr[(i - 9):i])
  }
  
  #+DM 1 Calculation#
  adx.tr$hidiff.1[1] <- NA
  adx.tr$lodiff.1[1] <- NA
  adx.tr$p.DM.1[1] <- NA
  
  for (i in 2:length(adx.tr$Close)) {
    adx.tr$hidiff.1[i] <- adx.tr$High[i] - adx.tr$High[i - 1]
  }
  for (i in 2:length(adx.tr$Close)) {
    adx.tr$lodiff.1[i] <- adx.tr$Low[i - 1] - adx.tr$Low[i]
  }
  
  for (i in 2:length(adx.tr$Close)) {
    if (adx.tr$hidiff.1[i] > adx.tr$lodiff.1[i]) {
      adx.tr$p.DM.1[i] <- max(adx.tr$hidiff.1[i],0)
    } else {
      adx.tr$p.DM.1[i] <- 0
    }
  }
  
  #-DM 1 Calculation#
  adx.tr$n.DM.1[1] <- NA
  for (i in 2:length(adx.tr$Close)) {
    if (adx.tr$lodiff.1[i] > adx.tr$hidiff.1[i]) {
      adx.tr$n.DM.1[i] <- max(adx.tr$lodiff.1[i],0)
    } else {
      adx.tr$n.DM.1[i] <- 0
    }
  }
  
  #TR14 Calculation#
  adx.tr$tr.14[1] <- NA
  adx.tr$tr.14[1:14] <- c(rep(NA,14))
  adx.tr$tr.14[15] <- sum(adx.tr$tr[2:15],na.rm = TRUE)
  for (i in 16:length(adx.tr$Close)) {
    adx.tr$tr.14[i] <-
      (adx.tr$tr.14[i - 1] - (adx.tr$tr.14[i - 1] / 14) + adx.tr$tr[i])
  }
  
  #+DM 14 Calculation#
  adx.tr$p.DM.14[1] <- NA
  adx.tr$p.DM.14[1:14] <- c(rep(NA,14))
  adx.tr$p.DM.14[15] <- sum(adx.tr$p.DM.1[2:15],na.rm = TRUE)
  for (i in 16:length(adx.tr$Close)) {
    adx.tr$p.DM.14[i] <-
      (adx.tr$p.DM.14[i - 1] - (adx.tr$p.DM.14[i - 1] / 14) + adx.tr$p.DM.1[i])
  }
  
  #-DM 14 Calculation#
  adx.tr$n.DM.14[1] <- NA
  adx.tr$n.DM.14[1:14] <- c(rep(NA,14))
  adx.tr$n.DM.14[15] <- sum(adx.tr$n.DM.1[2:15],na.rm = TRUE)
  for (i in 16:length(adx.tr$Close)) {
    adx.tr$n.DM.14[i] <-
      (adx.tr$n.DM.14[i - 1] - (adx.tr$n.DM.14[i - 1] / 14) + adx.tr$n.DM.1[i])
  }
  
  #+DI 14 Calculation#
  adx.tr$p.DI.14[1] <- NA
  adx.tr$p.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(adx.tr$Close)) {
    adx.tr$p.DI.14[i] <- (100 * (adx.tr$p.DM.14[i] / adx.tr$tr.14[i]))
  }
  
  #-DI 14 Calculation#
  adx.tr$n.DI.14[1] <- NA
  adx.tr$n.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(adx.tr$Close)) {
    adx.tr$n.DI.14[i] <- (100 * (adx.tr$n.DM.14[i] / adx.tr$tr.14[i]))
  }
  
  #DI 14 Diff Calculation#
  adx.tr$diff.DI.14[1] <- NA
  adx.tr$diff.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(adx.tr$Close)) {
    adx.tr$diff.DI.14[i] <- abs(adx.tr$p.DI.14[i] - adx.tr$n.DI.14[i])
  }
  
  #DI 14 Sum Calculation#
  adx.tr$sum.DI.14[1] <- NA
  adx.tr$sum.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(adx.tr$Close)) {
    adx.tr$sum.DI.14[i] <- adx.tr$p.DI.14[i] + adx.tr$n.DI.14[i]
  }
  
  #DX Calculation#
  adx.tr$DX[1] <- NA
  adx.tr$DX[1:14] <- c(rep(NA,14))
  for (i in 15:length(adx.tr$Close)) {
    adx.tr$DX[i] <- (100 * (adx.tr$diff.DI.14[i] / adx.tr$sum.DI.14[i]))
  }
  
  #ADX Calculation#
  adx.tr$ADX[1] <- NA
  adx.tr$ADX[1:27] <- c(rep(NA,27))
  adx.tr$ADX[28] <- mean(adx.tr$DX[15:28])
  for (i in 29:length(adx.tr$Close)) {
    adx.tr$ADX[i] <- (((adx.tr$ADX[i - 1] * 13) + adx.tr$DX[i]) / 14)
  }
  
  #20 Day Moving Average#
  require(zoo)
  
  adx.tr$MA.20[1] <- NA
  adx.tr$MA.20[1:19] <- c(rep(NA,19))
  adx.tr$MA.20[20:length(adx.tr$Close)] <-
    rollmean(adx.tr$Close, 20)
  
  #20 Day Moving STD#
  adx.tr$STD.20[1] <- NA
  adx.tr$STD.20[1:19] <- c(rep(NA,19))
  for (i in 20:length(adx.tr$Close)) {
    adx.tr$STD.20[i] <- sd(adx.tr$Close[(i - 19):i]) * (sqrt(19 / 20))
  }
  
  #Bollinger Bands(20,2)#
  adx.tr$up.Bol <- adx.tr$MA.20 + (2 * adx.tr$STD.20)
  adx.tr$down.Bol <- adx.tr$MA.20 - (2 * adx.tr$STD.20)
  adx.tr$Bollinger <- adx.tr$up.Bol - adx.tr$down.Bol
  
  #A-D Line#
  adx.tr$MF.Mult <-
    ((adx.tr$Close - adx.tr$Low) - (adx.tr$High - adx.tr$Close)) / (adx.tr$High - adx.tr$Low)
  
  adx.tr$MF.Value <- adx.tr$MF.Mult * adx.tr$Volume
  
  adx.tr$ADLine[1] <- adx.tr$MF.Value[1]
  for (i in 20:length(adx.tr$Close)) {
    adx.tr$ADLine[i] <- (adx.tr$ADLine[i - 1] + adx.tr$MF.Value[i])
  }
  
  #CCI 20-Day#
  require(lsr)
  
  adx.tr$Typical.Price <-
    (adx.tr$High + adx.tr$Low + adx.tr$Close) / 3
  
  adx.tr$SMA.20[1] <- NA
  adx.tr$SMA.20[1:19] <- c(rep(NA,19))
  adx.tr$SMA.20[20:length(adx.tr$Close)] <-
    rollmean(adx.tr$Typical.Price, 20)
  
  adx.tr$MAD.20[1] <- NA
  adx.tr$MAD.20[1:19] <- c(rep(NA,19))
  for (i in 20:length(adx.tr$Close)) {
    adx.tr$MAD.20[i] <- aad(adx.tr$Typical.Price[(i - 19):i])
  }
  
  adx.tr$CCI.20 <-
    (adx.tr$Typical.Price - adx.tr$SMA.20) / (.015 * adx.tr$MAD.20)
  
  #CMF 20-Day#
  adx.tr$CMF.20[1] <- NA
  adx.tr$CMF.20[1:19] <- c(rep(NA,19))
  adx.tr$CMF.20[20:length(adx.tr$Close)] <-
    rollmean(adx.tr$MF.Value, 20) / rollmean(adx.tr$Volume, 20)
  
  #OBV Calculation#
  adx.tr$OBV[1] <- 0
  for (i in 2:length(adx.tr$Close)) {
    if (adx.tr$Close[i] > adx.tr$Close[i - 1]) {
      adx.tr$OBV[i] <- (adx.tr$OBV[i - 1] + adx.tr$Volume[i])
    }
    if (adx.tr$Close[i] < adx.tr$Close[i - 1]) {
      adx.tr$OBV[i] <- (adx.tr$OBV[i - 1] - adx.tr$Volume[i])
    }
    else {
      adx.tr$OBV[i] <- adx.tr$OBV[i - 1]
    }
  }
  
  #Money Flow Index#
  adx.tr$updown[1] <- NA
  for (i in 2:length(adx.tr$Close)) {
    if (adx.tr$Typical.Price[i] > adx.tr$Typical.Price[i - 1]) {
      adx.tr$updown[i] <- 1
    }
    else {
      adx.tr$updown[i] <- -1
    }
  }
  
  adx.tr$Raw.Flow <- adx.tr$Typical.Price * adx.tr$Volume
  
  adx.tr$Pos.Flow.1[1] <- NA
  for (i in 2:length(adx.tr$Close)) {
    if (adx.tr$updown[i] > 0) {
      adx.tr$Pos.Flow.1[i] <- adx.tr$Raw.Flow[i]
    }
    else {
      adx.tr$Pos.Flow.1[i] <- 0
    }
  }
  
  adx.tr$Neg.Flow.1[1] <- NA
  for (i in 2:length(adx.tr$Close)) {
    if (adx.tr$updown[i] < 0) {
      adx.tr$Neg.Flow.1[i] <- adx.tr$Raw.Flow[i]
    }
    else {
      adx.tr$Neg.Flow.1[i] <- 0
    }
  }
  
  adx.tr$Pos.Flow.14[1] <- NA
  adx.tr$Pos.Flow.14[2:14] <- NA
  for (i in 15:length(adx.tr$Close)) {
    adx.tr$Pos.Flow.14[i] <- sum(adx.tr$Pos.Flow.1[(i - 13):i])
  }
  
  adx.tr$Neg.Flow.14[1] <- NA
  adx.tr$Neg.Flow.14[2:14] <- NA
  for (i in 15:length(adx.tr$Close)) {
    adx.tr$Neg.Flow.14[i] <- sum(adx.tr$Neg.Flow.1[(i - 13):i])
  }
  
  adx.tr$Ratio.Flow.14 <- adx.tr$Pos.Flow.14 / adx.tr$Neg.Flow.14
  
  adx.tr$Index.Flow.14 <- (100 - (100 / (1 + adx.tr$Ratio.Flow.14)))
  
  #Stochastic Oscillator 14-Day#
  adx.tr$hi.14[1] <- NA
  adx.tr$hi.14[2:13] <- NA
  for (i in 14:length(adx.tr$Close)) {
    adx.tr$hi.14[i] <- max(adx.tr$High[(i - 13):i])
  }
  
  adx.tr$lo.14[1] <- NA
  adx.tr$lo.14[2:13] <- NA
  for (i in 14:length(adx.tr$Close)) {
    adx.tr$lo.14[i] <- min(adx.tr$Low[(i - 13):i])
  }
  
  adx.tr$lo.14[1] <- NA
  adx.tr$lo.14[2:13] <- NA
  adx.tr$Osc.14 <-
    ((adx.tr$Close - adx.tr$lo.14) / (adx.tr$hi.14 - adx.tr$lo.14)) * 100
  
  #Williams %R 14-Day#
  adx.tr$Will.14 <-
    ((adx.tr$hi.14 - adx.tr$Close) / (adx.tr$hi.14 - adx.tr$lo.14)) * (-100)
  
  #EMA 12, 20, 26 Day#
  adx.tr$EMA.12[1] <- NA
  adx.tr$EMA.12[2:11] <- NA
  adx.tr$EMA.12[12] <- mean(adx.tr$Close[1:12])
  for (i in 13:length(adx.tr$Close)) {
    adx.tr$EMA.12[i] <-
      (((2 / 13) * (adx.tr$Close[i] - adx.tr$EMA.12[i - 1])) + adx.tr$EMA.12[(i-1)])
  }
  
  adx.tr$EMA.20[1] <- NA
  adx.tr$EMA.20[2:19] <- NA
  adx.tr$EMA.20[20] <- mean(adx.tr$Close[1:20])
  for (i in 21:length(adx.tr$Close)) {
    adx.tr$EMA.20[i] <-
      (((2 / 21) * (adx.tr$Close[i] - adx.tr$EMA.20[i - 1])) + adx.tr$EMA.20[(i-1)])
  }
  
  adx.tr$EMA.26[1] <- NA
  adx.tr$EMA.26[2:25] <- NA
  adx.tr$EMA.26[26] <- mean(adx.tr$Close[1:26])
  for (i in 27:length(adx.tr$Close)) {
    adx.tr$EMA.26[i] <-
      (((2 / 27) * (adx.tr$Close[i] - adx.tr$EMA.26[i - 1])) + adx.tr$EMA.26[(i-1)])
  }
  
  #KC Bands#
  adx.tr$kc.upper <- (adx.tr$EMA.20 + (2 * adx.tr$atr))
  adx.tr$kc.lower <- (adx.tr$EMA.20 - (2 * adx.tr$atr))
  adx.tr$kc.width <- adx.tr$kc.upper - adx.tr$kc.lower
  
  #Bollinger/Keltner Comparison#
  adx.tr$bb.kc[1] <- NA
  adx.tr$bb.kc[2:20] <- NA
  for (i in 21:length(adx.tr$Close)) {
    if ((adx.tr$up.Bol[i] < adx.tr$kc.upper[i]) &
        (adx.tr$down.Bol[i] > adx.tr$kc.lower[i])) {
      adx.tr$bb.kc[i] <- 1
    }
    else {
      adx.tr$bb.kc[i] <- 0
    }
  }
  
  
  #CCI Cross#
  adx.tr$CCI.cross[1] <- NA
  adx.tr$CCI.cross[2:20] <- NA
  for (i in 21:length(adx.tr$Close)) {
    if ((try(adx.tr$CCI.20[i] > 100)) &
        try((adx.tr$CCI.20[i - 1] < 100))) {
      adx.tr$CCI.cross[i] <- 1
    }
    else {
      adx.tr$CCI.cross[i] <- 0
    }
  }
  
  #CCI Above#
  adx.tr$CCI.above[1] <- NA
  adx.tr$CCI.above[2:20] <- NA
  for (i in 21:length(adx.tr$Close)) {
    if (adx.tr$CCI.20[i] > 100) {
      adx.tr$CCI.above[i] <- 1
    }
    else {
      adx.tr$CCI.above[i] <- 0
    }
  }
  
  #DI Cross#
  adx.tr$DI.cross[1] <- NA
  adx.tr$DI.cross[2:15] <- NA
  for (i in 16:length(adx.tr$Close)) {
    if ((adx.tr$p.DI.14[i] > adx.tr$n.DI.14[i]) &
        (adx.tr$p.DI.14[(i - 1)] < adx.tr$n.DI.14[(i - 1)])) {
      adx.tr$DI.cross[i] <- 1
    }
    else {
      adx.tr$DI.cross[i] <- 0
    }
  }
  
  #DI Above#
  adx.tr$DI.above[1] <- NA
  adx.tr$DI.above[2:15] <- NA
  for (i in 16:length(adx.tr$Close)) {
    if (adx.tr$p.DI.14[i] > adx.tr$n.DI.14[i]) {
      adx.tr$DI.above[i] <- 1
    }
    else {
      adx.tr$DI.above[i] <- 0
    }
  }
  
  
  #MACD Calculation#
  adx.tr$MACD[1] <- NA
  adx.tr$MACD[2:25] <- NA
  for (i in 26:length(adx.tr$Close)) {adx.tr$MACD[i] <- adx.tr$EMA.12[i] - adx.tr$EMA.26[i]}
  
  #MACD Signal#
  adx.tr$MACD.signal[1] <- NA
  adx.tr$MACD.signal[2:33] <- NA
  adx.tr$MACD.signal[34] <- mean(adx.tr$MACD[26:34])
  for (i in 35:length(adx.tr$Close)) {
    adx.tr$MACD.signal[i] <-
      (((2 / 27) * (adx.tr$MACD[i] - adx.tr$MACD.signal[i - 1])) + adx.tr$MACD.signal[(i-1)])
  }
  
  #MA Cross#
  adx.tr$MA.cross[1] <- NA
  adx.tr$MA.cross[2:34] <- NA
  for (i in 35:length(adx.tr$Close)) {
    if ((adx.tr$MACD[i] > adx.tr$MACD.signal[i]) &
        (adx.tr$MACD[(i - 1)] < adx.tr$MACD.signal[(i - 1)])) {
      adx.tr$MA.cross[i] <- 1
    }
    else {
      adx.tr$MA.cross[i] <- 0
    }
  }
  
  #MA Above#
  adx.tr$MA.above[1] <- NA
  adx.tr$MA.above[2:34] <- NA
  for (i in 35:length(adx.tr$Close)) {
    if (adx.tr$MACD[i] > adx.tr$MACD.signal[i]) {
      adx.tr$MA.above[i] <- 1
    }
    else {
      adx.tr$MA.above[i] <- 0
    }
  }
  
  #CCI, DI, MA Cross 3#
  adx.tr$CCI.cross.3[1] <- NA
  adx.tr$CCI.cross.3[2:22] <- NA
  for ( i in 23:length(adx.tr$Close)) {
    adx.tr$CCI.cross.3[i] <- adx.tr$CCI.cross[i] + adx.tr$CCI.cross[i-1] + adx.tr$CCI.cross[i-2]}
  
  adx.tr$DI.cross.3[1] <- NA
  adx.tr$DI.cross.3[2:22] <- NA
  for ( i in 23:length(adx.tr$Close)) {
    adx.tr$DI.cross.3[i] <- adx.tr$DI.cross[i] + adx.tr$DI.cross[i-1] + adx.tr$DI.cross[i-2]}
  
  adx.tr$MA.cross.3[1] <- NA
  adx.tr$MA.cross.3[2:22] <- NA
  for ( i in 23:length(adx.tr$Close)) {
    adx.tr$MA.cross.3[i] <- adx.tr$MA.cross[i] + adx.tr$MA.cross[i-1] + adx.tr$MA.cross[i-2]}
  
  #Price KC#
  adx.tr$price.kc[1] <- NA
  adx.tr$price.kc[2:19] <- NA
  for (i in 20:length(adx.tr$Close)) {
    if (adx.tr$Close[i] > adx.tr$kc.upper[i]) {adx.tr$price.kc[i] <- 1} 
    else {adx.tr$price.kc[i] <- 0}
  }
  
  #Buy Triple Cross 3#
  adx.tr$buy.three.cross[1] <- NA
  adx.tr$buy.three.cross[2:36] <- NA
  for (i in 37:length(adx.tr$Close)) {
    if((adx.tr$bb.kc[i] == 1) & (adx.tr$CCI.cross.3[i] == 1) & (adx.tr$DI.cross.3[i] == 1) & (adx.tr$MA.cross.3[i] == 1)) {
      adx.tr$buy.three.cross[i] <- 1}
    else { adx.tr$buy.three.cross[i] <- 0}
  }
  
  #Buy Roll DI Cross 3 (DI Roll = 6 Days)#
  adx.tr$buy.roll.DI[1] <- NA
  adx.tr$buy.roll.DI[2:36] <- NA
  for (i in 37:length(adx.tr$Close)) {
    if((adx.tr$bb.kc[i] == 1) & (adx.tr$CCI.cross.3[i] == 1) & (adx.tr$MA.cross.3[i] == 1) & (adx.tr$DI.cross.3[i] == 1)
       & (adx.tr$DI.cross.3[i-1] == 1) & (adx.tr$DI.cross.3[i-2] == 1) & (adx.tr$DI.cross.3[i-3] == 1)
       & (adx.tr$DI.cross.3[i-4] == 1) & (adx.tr$DI.cross.3[i-5] == 1)) {
      adx.tr$buy.roll.DI[i] <- 1}
    else { adx.tr$buy.roll.DI[i] <- 0}
  }
  
  #Buy MA Above#
  adx.tr$buy.MA.above[1] <- NA
  adx.tr$buy.MA.above[2:36] <- NA
  for (i in 37:length(adx.tr$Close)) {
    if((adx.tr$price.kc[i] == 1) & (adx.tr$CCI.cross.3[i] == 1) & (adx.tr$MA.above[i] == 1) & (adx.tr$DI.cross.3[i] == 1)) {
      adx.tr$buy.MA.above[i] <- 1}
    else { adx.tr$buy.MA.above[i] <- 0}
  }
  
  #Buy Triple Above#
  adx.tr$buy.triple.above[1] <- NA
  adx.tr$buy.triple.above[2:36] <- NA
  for (i in 37:length(adx.tr$Close)) {
    if((adx.tr$bb.kc[i] == 1) & (adx.tr$CCI.above[i] == 1) & (adx.tr$MA.above[i] == 1) & (adx.tr$DI.above[i] == 1)) {
      adx.tr$buy.triple.above[i] <- 1}
    else { adx.tr$buy.triple.above[i] <- 0}
  }
  
  #Buy Triple Above Modified#
  adx.tr$buy.triple.mod[1] <- NA
  adx.tr$buy.triple.mod[2:39] <- NA
  for (i in 40:length(adx.tr$Close)) {
    if((adx.tr$buy.triple.above[i] == 1) & (adx.tr$CCI.above[i-1] == 1) & (adx.tr$CCI.above[i-2] == 1) & (adx.tr$CCI.above[i-3] == 1)) {
      adx.tr$buy.triple.mod[i] <- 1}
    else { adx.tr$buy.triple.mod[i] <- 0}
  }
  
  
  write.csv(adx.tr, output.file, row.names = FALSE)
  
  return(adx.tr)
  
}
