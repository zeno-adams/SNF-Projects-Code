# Install and load the necessary  packages

packages <- c("EIAdata", "xts", "ggplot2", "plotly", "mFilter", "readxl", "sjPlot", "huxtable") # Install EIA package to retrive WTI crude oil spot prices


lapply(packages, library, character.only = TRUE)



# Load futures data

fut <- read.table("individual_futures_CLcrude_new_2020.txt", header = TRUE, dec = ",")



# Remove the NQM columns which are EMINI futures

fut <- fut[, -grep("NCLC.", names(fut))]



# Formatting dates as readable date

fut$date <- as.Date(fut$date, format = "%d/%m/%Y")



# Vector with observable dates

date <- fut$date



# First, we need to bring the columns in fut into the right order:

yrs <- 32 # from 1989 to 2020. Actually, we are only interested  in the period 1990 - 2020 but need additional obs

fut.codes <- paste(rep("NCL", yrs*12), rep(c("01","02","03","04","05","06","07","08","09","10","11","12"), yrs),
                   
                   rep(c(89:99,"00","01","02","03","04","05","06","07","08","09",10:20), rep(12,yrs)), sep = "")


# Organised future dataset

fut.codes <- fut.codes[fut.codes %in% names(fut)]
fut2 <- fut[, fut.codes]



# Create time series object of fut2

fut2xts <- xts(fut2, order.by = date)

Spot.Data <- read.table("all_spot_new_daily_2020.txt", header = TRUE, dec = ",")
Spot.Data$Date <- base::as.Date(as.character(Spot.Data$Date),format="%d/%m/%Y")
Spot <- Spot.Data$oil.spot
Spot <- xts(Spot, order.by = Spot.Data$Date)

# Merge spot and future data according to fut dates

fut3xts <- merge.xts(Spot, fut2xts, join = "right")



# Create data frame to populate with data to regress

regdata <- data.frame(matrix(ncol = 26, nrow = nrow(fut4)))

colnames(regdata) <- c("Date" , "Spot", sprintf("F%02d", 1:12) , sprintf("dt%02d", 1:12))

regdata$Date <- as.Date(fut4$Date, format = "%d.%m.%Y")



Actualcontract <- as.numeric(2) # 2 because date and spot in the first 2 columns 


# Retrieve and populate the regdata data frame
for (i in 1:nrow(fut4)) {
  
  regdata$Spot[i] <- fut4$Spot[i]
  
  if(regdata$Date[i] < tail(fut4$Date[!is.na(fut4[, Actualcontract + 1])], n=1)) { # check if futures is still trading (smaller than maturity date)
    
    for(j in 1:12) {
      
      regdata[i, 2+j] <- fut4[i, Actualcontract + j]
      
      if(!is.na(fut4[i,Actualcontract + j])) {
        
        regdata[i, 14+j] <- tail(fut4$Date[!is.na(fut4[, Actualcontract + j])], n=1) - fut4$Date[i] # time to maturity in days
        
      }
      
    }
    
  } else if (regdata$Date[i] == tail(fut4$Date[!is.na(fut4[, Actualcontract + 1])], n=1)) {
    
    # Consider next contract as today is the the maturity date (settlment != last price)
    
    Actualcontract <- Actualcontract + 1
    
    
    
    # Stop if maturity is beyond the observation period in the 12th contract
    
    if(tail(fut4$Date[!is.na(fut4[, Actualcontract + 12])], 1) == tail(fut4$Date, 1)) 
      
      break
    
    
    
    # Otherwise continue to populate the regdata data frame
    
    for(j in 1:12) {
      
      regdata[i, 2+j] <- fut4[i, Actualcontract + j]
      
      if(!is.na(fut4[i,Actualcontract + j])) {
        
        regdata[i, 14+j] <- tail(fut4$Date[!is.na(fut4[, Actualcontract + j])], n=1) - fut4$Date[i]
        
      }
      
    }
    
  }
  
}



# Remove rows where not all data are available (all non na) or matury 12th contract
# is beyond observation period

# removed.dates <- regdata[rowSums(!is.na(regdata[, 2:14])) == 0, ]

regdata <- regdata[rowSums(is.na(regdata[, 2:14])) == 0, ]


##
fut <- read.table("individual_futures_CLcrude_new_2020.txt", header = TRUE, dec = ",")

# convert the date into Date class in order to make it readable as an xts object:
date <- as.Date(as.character(fut$date), format = "%d/%m/%Y")

tmp <- data.frame(date = date, tmp.vals = fut$NCL0118)
tmp <- xts(tmp$tmp.vals, tmp$date)

# official GSCI roll dates:
roll.dates <- index(do.call(rbind, lapply(split(tmp[-1], "months"), function(x) x[5:9]))) #[-c(1)]



# Linear estimation of speculation as Futures price - Estimated Futures price for the nearest future contract (not mean as maturity date may not be between 0 and dt02)

lin.est <- data.frame(Date = regdata$Date, Est.F01 = regdata$Spot + (regdata$F02 - regdata$Spot) / regdata$dt02 * regdata$dt01)

lin.est$Speculation <- regdata$Spot - lin.est$Est.F01



# Create data.frame for Interpolation with spline

Interpolation <- data.frame(matrix(ncol = 16))

colnames(Interpolation) <- c("Date", "Est.F01", "Speculation", "Abs.Speculation", "Perc.Speculation",
"Abs.Perc.Speculation", "Roll Period", "Speculation Rolling Out",
"Speculation Rolling In", "F01", "abs.in.out", "net.in.out", "net.out.in", "abs.in.out.smooth", "net.in.out.smooth", "net.out.in.smooth") 

Interpolation$Date <- as.Date(Interpolation$Date)



# Interpolation with spline without considering F01

for (i in 1:nrow(regdata)) {
  
  Interpolation[i ,1] <- regdata$Date[i]
  
  Interpolation[i, 2] <- spline(c(0,NA,2:12), c(regdata$Spot[i], NA, t(regdata[i, 4:14])), method = "natural", xout = 1)$y
  
  Interpolation[i ,3] <- regdata$F01[i] - Interpolation[i, 2]
  
  Interpolation[i ,4] <- abs(Interpolation[i ,3])
  
  Interpolation[i, 5] <- Interpolation[i ,3] / Interpolation[i, 2]
  
  Interpolation[i, 6] <- abs(Interpolation[i, 5])

if (regdata$Date[i] %in% roll.dates)  {
Interpolation[i, 7] <- as.character(substr(regdata$Date[i],1,7))
# Out (should be negative):
# money moving out should push F01 below the spline price
Interpolation[i, 8] <-  regdata$F01[i] - spline(c(0,NA,2:12), c(regdata$Spot[i], NA, t(regdata[i, 4:14])), method = "natural", xout = 1)$y
# In (should be positive):
# money flowing in should push F02 above the spline price
Interpolation[i, 9] <-  regdata$F02[i] - spline(c(0,1,NA,3:12), c(regdata$Spot[i], regdata[i, 3], NA, t(regdata[i, 5:14])), method = "natural", xout = 2)$y

Interpolation[i, 10] <-  regdata[i,3]
}
if (i %% 100 == 0)
cat("Filling row number ", i, "of", nrow(regdata), "\n")
}


# ---------------------------------------------------------------
# Plot Going-In and Going Out speculation measures:
Interpolation1 <- Interpolation[!is.na(Interpolation$`Speculation Rolling In`),]
Interpolation2<- aggregate(Interpolation1, by = list(Interpolation1$`Roll Period`), FUN = mean)

Interpolation2$Date <- Interpolation2$Group.1

index <- 1:nrow(Interpolation2)
bw <- 0.05 # loess bandwith
n <- nrow(Interpolation2)
date <- Interpolation2$Date


# --------------------
# compute average speculation values to be reported in the paper:
prefin <- 1:which(Interpolation2$Date == "2008-08")
mean(Interpolation2$`Speculation Rolling In`[prefin], na.rm = TRUE)
mean(Interpolation2$`Speculation Rolling Out`[prefin], na.rm = TRUE)

fin <- which(Interpolation2$Date == "2008-09"):which(Interpolation2$Date == "2014-06")
mean(Interpolation2$`Speculation Rolling In`[fin], na.rm = TRUE)
mean(Interpolation2$`Speculation Rolling Out`[fin], na.rm = TRUE)
# --------------------


quartz()
plot(Interpolation2$`Speculation Rolling In`, type = "n", xaxt = "n", 
ylab = "Deviation from Smooth Term Structure", xlab = "", cex.axis = 1.2, cex.lab = 1.2,
ylim = c(-1.5,1.5))
#grid(20,10, grey(0.6))
axis(1, at = c(1,1/4*n,1/2*n,3/4*n,n),
c(date[1],date[1/4*n],date[1/2*n],date[3/4*n],date[n]), cex.axis = 1.2)
xx <- c(1:n,n:1)
polygon(xx, c(rep(-3, n),rep(3,n)), col = grey(0.9), border = NA)
n1 <- which(date == "2004-01") ; n1
n2 <- which(date == "2014-06") ; n2
n3 <- length(n1:n2) ; n3
xx2 <- c(n1:n2,n2:n1)
polygon(xx2, c(rep(-3, n3),rep(3,n3)), col = grey(0.7), border = NA)
# grid(10,10, grey(0.6))
# abline(h = 0, lwd = 2, col = 2, lty = 2)
text(90, 2-0.5, labels = "Pre-Financialization", cex = 1.3)
text(230, 2-0.5, labels = "Financialization", cex = 1.3)
text(330, 1.85-0.5, labels = "De- \n Financiali- \n zation", cex = 1.3)
out.smooth <- predict(loess(`Speculation Rolling Out` ~ index , data = Interpolation2,
span = bw), se = TRUE)
out.smooth.fit <- out.smooth$fit
out.smooth.lwr <- out.smooth$fit - 1.96*out.smooth$se.fit
out.smooth.upr <- out.smooth$fit + 1.96*out.smooth$se.fit
polygon(xx, c(out.smooth.lwr,rev(out.smooth.upr)), col = rgb(0.9,0,0,0.4), border = NA)
points(Interpolation2$`Speculation Rolling Out`, pch = 16, col = "indianred1", cex = 0.7)
lines(out.smooth.fit, x = index, col = "darkred", lwd = 3)
in.smooth <- predict(loess(`Speculation Rolling In` ~ index , data = Interpolation2,
span = bw), se = TRUE)
in.smooth.fit <- in.smooth$fit
in.smooth.lwr <- in.smooth$fit - 1.96*in.smooth$se.fit
in.smooth.upr <- in.smooth$fit + 1.96*in.smooth$se.fit
polygon(xx, c(in.smooth.lwr,rev(in.smooth.upr)), col = rgb(0,0.9,0,0.4), border = NA)
points(Interpolation2$`Speculation Rolling In`, pch = 16, col = "lightgreen", cex = 0.7)
lines(in.smooth.fit, x = index, col = "springgreen4", lwd = 3)
legend("bottomleft", c("Speculation Rolling into F02", "Speculation Rolling out of F01"),
col = c("springgreen4","darkred"), lwd = 3, inset = 0.02, bg = "antiquewhite1")
abline(h = 0, col = 2, lwd = 3, lty = 2)
box()
quartz.save("Speculation rolling out and in.jpg")
dev.off()

# create absolute total speculation as the sum of the absolute values of going in and going out:
bw <- 0.2 # loess bandwith
Interpolation2$abs.in.out <- abs(Interpolation2$`Speculation Rolling In`) +
  abs(Interpolation2$`Speculation Rolling Out`)
# Alternatively: Precentage instead of absolute USD deviation
# Interpolation2$abs.perc.in.out <- (abs(Interpolation2$`Speculation Rolling in`) +
#   abs(Interpolation2$`Speculation Rolling Out`))/Interpolation2$F01
quartz()
plot(Interpolation2$abs.in.out, type = "n", xaxt = "n", ylim = c(0,2),
ylab = "Absolute Aggregate Speculation", xlab = "", cex.axis = 1.2, cex.lab = 1.2)
grid(20,10, grey(0.6))
axis(1, at = c(1,1/4*n,1/2*n,3/4*n,n),
c(date[1],date[1/4*n],date[1/2*n],date[3/4*n],date[n]), cex.axis = 1.2)
points(Interpolation2$abs.in.out, pch = 16, col = grey(0.4), cex = 0.7)
abs.in.out.smooth <- predict(loess(abs.in.out ~ index , data = Interpolation2, span = bw))
lines(abs.in.out.smooth, x = index, col = "dodgerblue", lwd = 2)
quartz.save("Absolute Aggregate Speculation (abs rolling out + abs rolling in).jpg")
dev.off()

# create net total speculation as the values of going in minus going out
# (going out should be negative):
bw <- 0.2 # loess bandwith
Interpolation2$net.in.out <- Interpolation2$`Speculation Rolling In` -
  Interpolation2$`Speculation Rolling Out`

# --------------------
# compute average speculation values to be reported in the paper:
prefin <- 1:which(Interpolation2$Date == "2008-08")
fin <- which(Interpolation2$Date == "2008-09"):which(Interpolation2$Date == "2014-06")
defin <- which(Interpolation2$Date == "2014-07"):n
mean(Interpolation2$net.in.out[prefin], na.rm = TRUE)
mean(Interpolation2$net.in.out[fin], na.rm = TRUE)
mean(Interpolation2$net.in.out[defin], na.rm = TRUE)
# --------------------

quartz()
plot(Interpolation2$net.in.out, type = "n", xaxt = "n", ylim = c(-0.5,1.5),
ylab = "Net Aggregate Speculation", xlab = "", cex.axis = 1.2, cex.lab = 1.2)
# grid(20,10, grey(0.6))
axis(1, at = c(1,1/4*n,1/2*n,3/4*n,n),
c(date[1],date[1/4*n],date[1/2*n],date[3/4*n],date[n]), cex.axis = 1.2)
xx <- c(1:n,n:1)
polygon(xx, c(rep(-3, n),rep(3,n)), col = grey(0.9), border = NA)
n1 <- which(date == "2004-01") ; n1
n2 <- which(date == "2014-06") ; n2
n3 <- length(n1:n2) ; n3
xx2 <- c(n1:n2,n2:n1)
polygon(xx2, c(rep(-3, n3),rep(3,n3)), col = grey(0.7), border = NA)
box()
net.in.out.smooth <- predict(loess(net.in.out ~ index , data = Interpolation2, span = bw),
se = TRUE)
net.in.out.smooth.fit <- net.in.out.smooth$fit
net.in.out.smooth.lwr <- net.in.out.smooth$fit - 1.96*net.in.out.smooth$se.fit
net.in.out.smooth.upr <- net.in.out.smooth$fit + 1.96*net.in.out.smooth$se.fit
polygon(xx, c(net.in.out.smooth.lwr,rev(net.in.out.smooth.upr)), col = rgb(0,0,0.9,0.3), border = NA)
points(Interpolation2$net.in.out, pch = 16, col = "steelblue4", cex = 0.7)
lines(net.in.out.smooth.fit, x = index, col = "steelblue4", lwd = 3)
abline(h = 0, lwd = 3, lty = 2, col = 2)
text(90, 2-0.5, labels = "Pre-Financialization", cex = 1.3)
text(230, 2-0.5, labels = "Financialization", cex = 1.3)
text(330, 1.85-0.5, labels = "De- \n Financiali- \n zation", cex = 1.3)
quartz.save("Net Aggregate Speculation (rolling in - rolling out).jpg")
dev.off()



# create roll effect total speculation as the values of going out plus going in
# (going out should be negative):
bw <- 0.2 # loess bandwith
Interpolation2$net.out.in <- Interpolation2$`Speculation Rolling Out` +
  Interpolation2$`Speculation Rolling In`
quartz()
plot(Interpolation2$net.out.in, type = "n", xaxt = "n", ylim = c(-0.5,2),
     ylab = "Net Aggregate Speculation", xlab = "", cex.axis = 1.2, cex.lab = 1.2)
grid(20,10, grey(0.6))
axis(1, at = c(1,1/4*n,1/2*n,3/4*n,n),
     c(date[1],date[1/4*n],date[1/2*n],date[3/4*n],date[n]), cex.axis = 1.2)
points(Interpolation2$net.out.in, pch = 16, col = grey(0.4), cex = 0.7)
net.out.in.smooth <- predict(loess(net.out.in ~ index , data = Interpolation2, span = bw))
lines(net.out.in.smooth, x = index, col = "red4", lwd = 2)
quartz.save("Net Aggregate Speculation (rolling out + rolling in).jpg")
dev.off()



Interpolation2[, 15] <- abs.in.out.smooth
Interpolation2[, 16] <- net.in.out.smooth
Interpolation2[, 17] <- net.out.in.smooth



# Load Risk Premium estimates and multiply by -1 to get the right RP

Risk.Premium <- read.table("Risk_premium_crude_2020.txt", header = TRUE, dec = ",")
colnames(Risk.Premium) <- c("Date", "RP_HW", "RP_WTI")

rownames(Risk.Premium) <- NULL


Risk.Premium <- Risk.Premium[rowSums(!is.na(Risk.Premium[, 2:length(Risk.Premium)])) > 0, ] # Remove blank rows

Risk.Premium[, 2:3] <- Risk.Premium[, 2:3] * -1  

Risk.Premium$Date <- as.Date.character(Risk.Premium$Date, format = "%Y%m%d")

Risk.Premium$Date[Risk.Premium$Date == "2013-03-29"] <- "2013-03-28" # Match IID Dates

Risk.Premium$RP_WTI_Trend <- as.vector(hpfilter(Risk.Premium$RP_WTI, freq = 1000000/(255^4)*52^4, type = "lambda")$trend) # freq = 1000000/(255^4)*52^4 for weekly data



# Plot Risk Premium estimates
quartz(width = 14, height = 5.5, bg = "white")

plot(Risk.Premium$Date[!is.na(Risk.Premium$RP_HW)], Risk.Premium$RP_HW[!is.na(Risk.Premium$RP_HW)], type = "l", main = "8-week Risk Premium", xlab = "Date", ylab = "Risk Premium")

plot(Risk.Premium$Date, Risk.Premium$RP_WTI, type = "l", main = "8-week Risk Premium", xlab = "Date", ylab = "Risk Premium")

lines(Risk.Premium$Date, Risk.Premium$RP_WTI_Trend, col = "red", lwd=2)

legend("bottomleft", legend = c("Risk Premium", "Trend Risk Premium"), col = c("black", "red"), lty = c(1, 1), lwd=2, cex = 1, bg = "white")

quartz.save("8-week Risk Premium.jpg", dpi = 600)

dev.off()


Norm_Risk.Premium <- cbind(Date = Risk.Premium$Date, as.data.frame(apply(Risk.Premium[,-1], 2, function (x) x/x[Risk.Premium$Date == Norm_date]*100)))

#Average Risk Premium 
Risk.Premium$Date <- format(Risk.Premium$Date, "%Y-%m-01")
Risk.Premium2<- aggregate(Risk.Premium, by = list(Risk.Premium$`Date`), FUN = mean)
Risk.Premium2$Date <- Risk.Premium2$Group.1
Risk.Premium2$Date <- as.character(substr(Risk.Premium2$Date,1,7))



# Correlation Matrix

#Comp.Dataset <- Reduce(function(x, y) merge(x, y, by = "Date"), list(Risk.Premium[,c("Date", "RP_WTI_Trend")]))

#colnames(Comp.Dataset) <- c("Date", "RP_WTI_Trend")


# No normality -> Kendall correlation as Spearman problem with ties -> not suitable

#Corr_IID.and.Risk <- cor.test(Comp.Dataset$IID.Value.Net.Long_WTI, Comp.Dataset$RP_WTI_Trend, method = "pearson") 

#Corr_IID.and.Speculation <- cor.test(Comp.Dataset$IID.Value.Net.Long_WTI, Comp.Dataset$HP_Speculation_trend, method = "pearson") 

# cor(Comp.Dataset[, -1], method = "pearson")

sjt.corr(Comp.Dataset[, -1], corr.method = "pearson", show.p = T, digits = 4, string.diag = rep(1, ncol(Comp.Dataset[, -1])))



# Load market data for model regressions

Market.Data <- read.table("all_data_new_monthly_2020.txt", header = TRUE, dec = ",")
Market.Data$Date <- base::as.Date(as.character(Market.Data$Date),format="%d/%m/%Y")



# Create the regression data model

Spot <- data.frame(Date = index(Spot), Spot) # Transform spot from xts to data.frame

#Regdata.model <-  Reduce(function(x, y) merge(x, y, by = "Date"), list(Risk.Premium2, Spot, Market.Data))
Regdata.model <-  Reduce(function(x, y) merge(x, y, by = "Date"), list(Market.Data))

Regdata.model$log.return.sp500 <- c(NA, diff(log(Regdata.model$sp500)))

#Regdata.model$log.spot.return <- c(NA, diff(log(Regdata.model$Spot)))
Regdata.model$log.spot.return <- c(NA, diff(log(Regdata.model$oil.spot)))

Regdata.model$Date <- as.character(substr(Regdata.model$Date,1,7))

library(fGarch)
vol0 <- garchFit(~garch(1,1), data = Regdata.model$log.spot.return[-1], trace = FALSE)
Regdata.model$return.vol <- c(NA,sqrt(vol0@h.t)*sqrt(12)*100)

Regdata.model <-  Reduce(function(x, y) merge(x, y, by = "Date"), list(Regdata.model, Risk.Premium2, Interpolation2))



#Graphs of return and volatility
quartz()
plot(Regdata.model$return.vol)
plot(Regdata.model$return.vol,type="l")
dev.off()

quartz()
plot(Regdata.model$log.spot.return)
plot(Regdata.model$log.spot.return,type="l")
dev.off()



#Chose the time window
limit.down <- as.character("2014-06") #1990-01; 2008-09; 2014-06
limit.up <- as.character("2020-07") #2008-09; 2014-06; 2020-07
Regdata.model2 <- Regdata.model[Regdata.model$Date >= limit.down & Regdata.model$Date < limit.up,]                                                                    

# Regression models (Spot, RP, Trend are computed at the end of the month vs market data @ beginning)

Model1 <- lm(log.spot.return ~ RP_WTI_Trend, data = Regdata.model2)

Model2 <- lm(log.spot.return ~ net.in.out.smooth, data = Regdata.model2)

Model3 <- lm(log.spot.return ~ RP_WTI_Trend + net.in.out.smooth + kilian + us.inv
            
            + tb3m + log.return.sp500 + vix + fx + corn.spot, data = Regdata.model2)


# Show result regression models

reg <-huxreg(Model1, Model2, Model3, coefs = c( "Intercept" = "(Intercept)", "Risk Premium" = "RP_WTI_Trend",
                                                        
                                                        "Speculation Trend" = "net.in.out.smooth", "Lutz Kilian Index" = "kilian", "US Crude Oil Inventory" = "us.inv",
                                                        
                                                        "US T-Bill 3-month" = "tb3m", "S&P 500 Log Return" = "log.return.sp500", "VIX" = "vix",  "USD FX" = "fx", "Corn Price" = "corn.spot"))

reg

                         
