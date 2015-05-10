# Current scenario:
# (1) NZD has higher interst rates than AUD or GBP
# (2) NZD exchange rate is historically high against AUD and GBP
 
# So, assuming interest rates stay constant but exchange rates eventually return to their long-term averages, we have two 
# possible strategies:
#   (a) wait for favorable exchange rates to transfer AUD & GBP => NZD, but earn poor interest in the meantime, or
# (b) transfer AUD & GBP => NZD immediately to capitalise on good interest rates in NZ, but lose out on exchnage rate. 
 
# Note that the longer we have to wait for exchange rates to return to their historic levels, the better the relative 
# ...performance of the "transfer now" appraoch will be (because interest will have longer to compound). 

#-------------------------------------------------------------------------------------------------------------------
#### ==>  So my question is: given (1) 'final' exchange rates, and (2) time until those exchange rates are realised, 
#         which strategy performs better?
#------------------------------------------------------------------------------------------------------------------

#Parameters
initialAUD <- 35000
initialGBP <- 5000

NZinterestRate <- 0.05
AUinterestRate <- 0.03
UKinterestRate <- 0.01

exchangeRateCurrent_NZAU <- 0.95
exchangeRateCurrent_NZUK <- 0.5
exchangeRateFuture_NZAU_1 <- 0.75
exchangeRateFuture_NZUK_1 <- 0.35    #2 diff scenarios for future exchange rates: (1) historic levels, and (2) more modest [and more likely] drops
exchangeRateFuture_NZAU_2 <- 0.90
exchangeRateFuture_NZUK_2 <- 0.45


#=============
#1. Function to calculate savings and interest earned by the end of each month, given starting savings, annual interest rate, and 
#...number of months to calculate over. 
#...*** Note month is input as a sequence, e.g. "1:12" ***
#...Returns a dataframe with month number and savings as values.
#=============

savingsPlusInterest <- function(months, initialSavings, interestRate){  

  interestRate_month <- (1 + interestRate)^(1/12)-1    #monthly interest rate to result in specified annual rate
  month <- months               
  savings <- vector()             #vector to store updated savings for each month
    savings[1] <- initialSavings  #will use loop to update savings each month based on monthly interest rate, but need starting value.
  
  for(i in 1:length(month)){
    savings[i+1] <- savings[i]*(1 + interestRate_month) #takes initial savings, and adds monthly interest rate
  }
  savings <- savings[-1] #method above results in savings vector being one element too long, cos first value of vector is seeded with 'initial 
                        #...savings'. So deleting this seeding value from vector.
  savingsDF <- data.frame(month=month, savings=savings)
  savingsDF
}


#=============
#2. use above function to calculate savings under different scenarios
#=============

###A. transfer all money into NZD now:--------------------------------------------------------------------------------------------

exchangeNowSavings_NZD <- savingsPlusInterest(months=1:60,                                   #project for 5 years
                                              initialSavings=initialAUD*(1/exchangeRateCurrent_NZAU) +  #need reciprocal of exchange rate
                                                initialGBP*(1/exchangeRateCurrent_NZUK),                #.. to go AUD -> NZD                            
                                              interestRate=NZinterestRate)              

qplot(x=month, y=savings, data=exchangeNowSavings_NZD, ylim=c(0,max(exchangeNowSavings_TOTAL$savings)))


###B. Exchange in 1yr; and exchange rates (GBP & AUD) reach historic levels at that point.------------------------------------------------------- 

AUDPreConversion <- savingsPlusInterest(months=1:12, initialSavings=initialAUD, interestRate=AUinterestRate)
  FinalAUDPreConversion <- AUDPreConversion[nrow(AUDPreConversion),2] #pulls out final AUD value pre conversion
    AUDPostConversion <- savingsPlusInterest(months=13:60, initialSavings=FinalAUDPreConversion*(1/exchangeRateFuture_NZAU_1), 
                                             interestRate=NZinterestRate)
    #above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
      exchangeLaterSavings_AUD <- rbind(AUDPreConversion, AUDPostConversion)
        qplot(x=month, y=savings, data=exchangeLaterSavings_AUD)

GBPPreConversion <- savingsPlusInterest(months=1:12, initialSavings=initialGBP, interestRate=UKinterestRate)
  FinalGBPPreConversion <- GBPPreConversion[nrow(GBPPreConversion),2] #pulls out final value pre conversion
    GBPPostConversion <- savingsPlusInterest(months=13:60, initialSavings=FinalGBPPreConversion*(1/exchangeRateFuture_NZUK_1), 
                                             interestRate=NZinterestRate)
    #above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
      exchangeLaterSavings_GBP <- rbind(GBPPreConversion, GBPPostConversion)
        qplot(x=month, y=savings, data=exchangeLaterSavings_GBP)

exchangeLaterSavings_1Yr_FutureRates1 <- merge(exchangeLaterSavings_AUD, exchangeLaterSavings_GBP, by='month')
exchangeLaterSavings_1Yr_FutureRates1$savings <- with(exchangeLaterSavings_1Yr_FutureRates1, savings.x + savings.y)
exchangeLaterSavings_1Yr_FutureRates1 <- subset(exchangeLaterSavings_1Yr_FutureRates1, select=c(month, savings)) #merging GBP & AUD DFs, 
                                                                                                                 #...then clc8 total savings
  qplot(x=month, y=savings, data=exchangeLaterSavings_1Yr_FutureRates1)


###C. Exchange in 1yr; and exchange rates (GBP & AUD) drop modestly ['FutureRates2' parameters above] at that point.-------------------------

AUDPreConversion <- savingsPlusInterest(months=1:12, initialSavings=initialAUD, interestRate=AUinterestRate)
FinalAUDPreConversion <- AUDPreConversion[nrow(AUDPreConversion),2] #pulls out final AUD value pre conversion
AUDPostConversion <- savingsPlusInterest(months=13:60, initialSavings=FinalAUDPreConversion*(1/exchangeRateFuture_NZAU_2), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_AUD <- rbind(AUDPreConversion, AUDPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_AUD)

GBPPreConversion <- savingsPlusInterest(months=1:12, initialSavings=initialGBP, interestRate=UKinterestRate)
FinalGBPPreConversion <- GBPPreConversion[nrow(GBPPreConversion),2] #pulls out final value pre conversion
GBPPostConversion <- savingsPlusInterest(months=13:60, initialSavings=FinalGBPPreConversion*(1/exchangeRateFuture_NZUK_2), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_GBP <- rbind(GBPPreConversion, GBPPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_GBP)

exchangeLaterSavings_1Yr_FutureRates2 <- merge(exchangeLaterSavings_AUD, exchangeLaterSavings_GBP, by='month')
exchangeLaterSavings_1Yr_FutureRates2$savings <- with(exchangeLaterSavings_1Yr_FutureRates2, savings.x + savings.y)
exchangeLaterSavings_1Yr_FutureRates2 <- subset(exchangeLaterSavings_1Yr_FutureRates2, select=c(month, savings)) #merging GBP & AUD DFs, 
#...then clc8 total savings
qplot(x=month, y=savings, data=exchangeLaterSavings_1Yr_FutureRates2)


###D. Exchange in 4yr; and exchange rates (GBP & AUD) reach historic levels at that point.------------------------------------------------------- 

AUDPreConversion <- savingsPlusInterest(months=1:48, initialSavings=initialAUD, interestRate=AUinterestRate)
FinalAUDPreConversion <- AUDPreConversion[nrow(AUDPreConversion),2] #pulls out final AUD value pre conversion
AUDPostConversion <- savingsPlusInterest(months=49:60, initialSavings=FinalAUDPreConversion*(1/exchangeRateFuture_NZAU_1), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_AUD <- rbind(AUDPreConversion, AUDPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_AUD)

GBPPreConversion <- savingsPlusInterest(months=1:48, initialSavings=initialGBP, interestRate=UKinterestRate)
FinalGBPPreConversion <- GBPPreConversion[nrow(GBPPreConversion),2] #pulls out final value pre conversion
GBPPostConversion <- savingsPlusInterest(months=49:60, initialSavings=FinalGBPPreConversion*(1/exchangeRateFuture_NZUK_1), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_GBP <- rbind(GBPPreConversion, GBPPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_GBP)

exchangeLaterSavings_4Yr_FutureRates1 <- merge(exchangeLaterSavings_AUD, exchangeLaterSavings_GBP, by='month')
exchangeLaterSavings_4Yr_FutureRates1$savings <- with(exchangeLaterSavings_4Yr_FutureRates1, savings.x + savings.y)
exchangeLaterSavings_4Yr_FutureRates1 <- subset(exchangeLaterSavings_4Yr_FutureRates1, select=c(month, savings)) #merging GBP & AUD DFs, 
#...then clc8 total savings
qplot(x=month, y=savings, data=exchangeLaterSavings_4Yr_FutureRates1)


###E. Exchange in 4yr; and exchange rates (GBP & AUD) drop modestly ['FutureRates2' parameters above] at that point.-------------------------

AUDPreConversion <- savingsPlusInterest(months=1:48, initialSavings=initialAUD, interestRate=AUinterestRate)
FinalAUDPreConversion <- AUDPreConversion[nrow(AUDPreConversion),2] #pulls out final AUD value pre conversion
AUDPostConversion <- savingsPlusInterest(months=49:60, initialSavings=FinalAUDPreConversion*(1/exchangeRateFuture_NZAU_2), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_AUD <- rbind(AUDPreConversion, AUDPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_AUD)

GBPPreConversion <- savingsPlusInterest(months=1:48, initialSavings=initialGBP, interestRate=UKinterestRate)
FinalGBPPreConversion <- GBPPreConversion[nrow(GBPPreConversion),2] #pulls out final value pre conversion
GBPPostConversion <- savingsPlusInterest(months=49:60, initialSavings=FinalGBPPreConversion*(1/exchangeRateFuture_NZUK_2), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_GBP <- rbind(GBPPreConversion, GBPPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_GBP)

exchangeLaterSavings_4Yr_FutureRates2 <- merge(exchangeLaterSavings_AUD, exchangeLaterSavings_GBP, by='month')
exchangeLaterSavings_4Yr_FutureRates2$savings <- with(exchangeLaterSavings_4Yr_FutureRates2, savings.x + savings.y)
exchangeLaterSavings_4Yr_FutureRates2 <- subset(exchangeLaterSavings_4Yr_FutureRates2, select=c(month, savings)) #merging GBP & AUD DFs, 
#...then clc8 total savings
qplot(x=month, y=savings, data=exchangeLaterSavings_4Yr_FutureRates2)


###F. Exchange in 1yr; and exchange rates (GBP & AUD) don't change at all!.-------------------------

AUDPreConversion <- savingsPlusInterest(months=1:12, initialSavings=initialAUD, interestRate=AUinterestRate)
FinalAUDPreConversion <- AUDPreConversion[nrow(AUDPreConversion),2] #pulls out final AUD value pre conversion
AUDPostConversion <- savingsPlusInterest(months=13:60, initialSavings=FinalAUDPreConversion*(1/exchangeRateCurrent_NZAU), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_AUD <- rbind(AUDPreConversion, AUDPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_AUD)

GBPPreConversion <- savingsPlusInterest(months=1:12, initialSavings=initialGBP, interestRate=UKinterestRate)
FinalGBPPreConversion <- GBPPreConversion[nrow(GBPPreConversion),2] #pulls out final value pre conversion
GBPPostConversion <- savingsPlusInterest(months=13:60, initialSavings=FinalGBPPreConversion*(1/exchangeRateCurrent_NZUK), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_GBP <- rbind(GBPPreConversion, GBPPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_GBP)

exchangeLaterSavings_1Yr_CurrentRates <- merge(exchangeLaterSavings_AUD, exchangeLaterSavings_GBP, by='month')
exchangeLaterSavings_1Yr_CurrentRates$savings <- with(exchangeLaterSavings_1Yr_CurrentRates, savings.x + savings.y)
exchangeLaterSavings_1Yr_CurrentRates <- subset(exchangeLaterSavings_1Yr_CurrentRates, select=c(month, savings)) #merging GBP & AUD DFs, 
#...then clc8 total savings
qplot(x=month, y=savings, data=exchangeLaterSavings_1Yr_CurrentRates)



###G. Exchange in 1yr; and exchange rates (GBP & AUD) don't change at all!.-------------------------

AUDPreConversion <- savingsPlusInterest(months=1:48, initialSavings=initialAUD, interestRate=AUinterestRate)
FinalAUDPreConversion <- AUDPreConversion[nrow(AUDPreConversion),2] #pulls out final AUD value pre conversion
AUDPostConversion <- savingsPlusInterest(months=49:60, initialSavings=FinalAUDPreConversion*(1/exchangeRateCurrent_NZAU), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_AUD <- rbind(AUDPreConversion, AUDPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_AUD)

GBPPreConversion <- savingsPlusInterest(months=1:48, initialSavings=initialGBP, interestRate=UKinterestRate)
FinalGBPPreConversion <- GBPPreConversion[nrow(GBPPreConversion),2] #pulls out final value pre conversion
GBPPostConversion <- savingsPlusInterest(months=49:60, initialSavings=FinalGBPPreConversion*(1/exchangeRateCurrent_NZUK), 
                                         interestRate=NZinterestRate)
#above code says 'convert to NZD at future exchange rate, and then earn NZ interest until end of projection'
exchangeLaterSavings_GBP <- rbind(GBPPreConversion, GBPPostConversion)
qplot(x=month, y=savings, data=exchangeLaterSavings_GBP)

exchangeLaterSavings_4Yr_CurrentRates <- merge(exchangeLaterSavings_AUD, exchangeLaterSavings_GBP, by='month')
exchangeLaterSavings_4Yr_CurrentRates$savings <- with(exchangeLaterSavings_4Yr_CurrentRates, savings.x + savings.y)
exchangeLaterSavings_4Yr_CurrentRates <- subset(exchangeLaterSavings_4Yr_CurrentRates, select=c(month, savings)) #merging GBP & AUD DFs, 
#...then clc8 total savings
qplot(x=month, y=savings, data=exchangeLaterSavings_4Yr_CurrentRates)


#=============
#3. plot!
#=============

#set up dataframe for ggplot
plotData <- rbind(exchangeNowSavings_NZD,
                  exchangeLaterSavings_1Yr_FutureRates1,
                  exchangeLaterSavings_1Yr_FutureRates2,
                  exchangeLaterSavings_4Yr_FutureRates1,
                  exchangeLaterSavings_4Yr_FutureRates2,
                  exchangeLaterSavings_1Yr_CurrentRates,
                  exchangeLaterSavings_4Yr_CurrentRates)
plotData$strategy <- c(rep('exchangeNow', times=nrow(exchangeNowSavings_NZD)),
                       rep('exchangeIn1Yr_FutureRates1', times=nrow(exchangeNowSavings_NZD)),
                       rep('exchangeIn1Yr_FutureRates2', times=nrow(exchangeNowSavings_NZD)),
                       rep('exchangeIn4Yr_FutureRates1', times=nrow(exchangeNowSavings_NZD)),
                       rep('exchangeIn4Yr_FutureRates2', times=nrow(exchangeNowSavings_NZD)),
                       rep('exchangeIn1Yr_CurrentRates', times=nrow(exchangeNowSavings_NZD)),
                       rep('exchangeIn4Yr_CurrentRates', times=nrow(exchangeNowSavings_NZD)))

#plot!
ggplot(data=plotData, aes(x=month, y=savings, col=strategy)) + geom_line(size=1.5)
