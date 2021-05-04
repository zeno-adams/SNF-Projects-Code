library(fGarch)
library(zoo)

# # ---------- data preparation ------------------------------ ------------
dat1 <- read.table("all_data_new_2018.txt", header = TRUE)
n <- nrow(dat1)


# compute real variables and percentage changes:
dat1$oil.spot.real <- dat1$oil.spot/dat1$us.cpi*100
dat1$loil.spot.real <- log(dat1$oil.spot.real)
dat1$dloil.spot <- c(NA, diff(log(dat1$oil.spot)))
dat1$heating.spot.real <- dat1$heating.spot/dat1$us.cpi*100
dat1$lheating.spot.real <- log(dat1$heating.spot.real)
dat1$dlheating.spot <- c(NA, diff(log(dat1$heating.spot)))
dat1$corn.spot.real <- dat1$corn.spot/dat1$us.cpi*100
dat1$lcorn.spot.real <- log(dat1$corn.spot.real)
dat1$dlcorn.spot <- c(NA, diff(log(dat1$corn.spot)))
dat1$gold.spot.real <- dat1$gold.spot/dat1$us.cpi*100
dat1$lgold.spot.real <- log(dat1$gold.spot.real)
dat1$dlgold.spot <- c(NA, diff(log(dat1$gold.spot)))
# # add placebo financial assets:
# dat1$ds_banks.real <- dat1$ds_banks/dat1$us.cpi*100
# dat1$lds_banks.real <- log(dat1$ds_banks.real)
# dat1$dlds_banks <- c(NA, diff(log(dat1$ds_banks)))
# dat1$ds_insurance.real <- dat1$ds_insurance/dat1$us.cpi*100
# dat1$lds_insurance.real <- log(dat1$ds_insurance.real)
# dat1$dlds_insurance <- c(NA, diff(log(dat1$ds_insurance)))
# dat1$msci_europe.real <- dat1$msci_europe/dat1$us.cpi*100
# dat1$lmsci_europe.real <- log(dat1$msci_europe.real)
# dat1$dlmsci_europe <- c(NA, diff(log(dat1$msci_europe)))
# # note that because the off-index commodities only have small volumes
# # on the spot market, we use here the continuous nearby futures
# # contract for simplicity.
# dat1$orange.exret.real <- dat1$orange.exret/dat1$us.cpi*100
# dat1$lorange.exret.real <- log(dat1$orange.exret.real)
# dat1$dlorange.exret <- c(NA, diff(log(dat1$orange.exret)))
# dat1$lumber.exret.real <- dat1$lumber.exret/dat1$us.cpi*100
# dat1$llumber.exret.real <- log(dat1$lumber.exret.real)
# dat1$dllumber.exret <- c(NA, diff(log(dat1$lumber.exret)))
# dat1$oats.exret.real <- dat1$oats.exret/dat1$us.cpi*100
# dat1$loats.exret.real <- log(dat1$oats.exret.real)
# dat1$dloats.exret <- c(NA, diff(log(dat1$oats.exret)))


dat1$us.inf <- dat1$cpi_yoy # c(NA,diff(log(dat1$us.cpi))*100) # inflation rate is in percent
dat1$tb3m.real <- dat1$tb3m - dat1$us.inf # annual real yield in percent
dat1$dtb3m.real <- c(NA, diff(dat1$tb3m.real))
dat1$dlus.inv <- c(NA,diff(log(dat1$us.inv)))
dat1$dlsp500 <- c(NA,diff(log(dat1$sp500)))
dat1$dlip <- c(NA,diff(log(dat1$ip)))
dat1$copper.spot.real <- dat1$copper.spot/dat1$us.cpi*100
dat1$dlcopper.spot <- c(NA, diff(log(dat1$copper.spot)))
dat1$dtb10y <- c(NA, diff(dat1$tb10y)) # percentage point change in the annual 10 year yield
dat1$tenyearspread <- dat1$tb10y - dat1$tb2y
dat1$dlfx <- c(NA,diff(log(dat1$fx)))
dat1$dlvix<- c(NA, diff(log(dat1$vix)))
# dat1$tb3m.real <- dat1$tenyearspread 

# since the time series of oats starts a bit later we cut the
# time window by 99 obs:
# dat1 <- dat1[which(!is.na(dat1$dloats.exret))[1]:n,]

# compute volatilities
vol0 <- garchFit(~garch(1,1), data = dat1$dloil.spot[-1], trace = FALSE) 
dat1$oil.spot.vol <-  c(NA,sqrt(vol0@h.t)*sqrt(12)*100)
vol0 <- garchFit(~garch(1,1), data = dat1$dlsp500[-1], trace = FALSE) 
dat1$sp500.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
vol0 <- garchFit(~garch(1,1), data = dat1$dlcopper.spot[-1], trace = FALSE) 
dat1$dlcopper.spot.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
vol0 <- garchFit(~garch(1,1), data = dat1$dlgold.spot[-1], trace = FALSE) 
dat1$dlgold.spot.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
vol0 <- garchFit(~garch(1,1), data = dat1$dlheating.spot[-1], trace = FALSE) 
dat1$dlheating.spot.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
vol0 <- garchFit(~garch(1,1), data = dat1$dlcorn.spot[-1], trace = FALSE) 
dat1$dlcorn.spot.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
# vol0 <- garchFit(~garch(1,1), data = dat1$dlds_banks[-1], trace = FALSE) 
# dat1$dlds_banks.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
# vol0 <- garchFit(~garch(1,1), data = dat1$dlds_insurance[-1], trace = FALSE) 
# dat1$dlds_insurance.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
# vol0 <- garchFit(~garch(1,1), data = dat1$dlmsci_europe[-1], trace = FALSE) 
# dat1$dlmsci_europe.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
# vol0 <- garchFit(~garch(1,1), data = dat1$dloats.exret[-1], trace = FALSE) 
# dat1$dloats.exret.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
# vol0 <- garchFit(~garch(1,1), data = dat1$dllumber.exret[-1], trace = FALSE) 
# dat1$dllumber.exret.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
# vol0 <- garchFit(~garch(1,1), data = dat1$dlorange.exret[-1], trace = FALSE) 
# dat1$dlorange.exret.vol <-  c(NA, sqrt(vol0@h.t)*sqrt(12)*100)
# 

# add change in slope and intercept to the data as indicated by the ZA (1992) test
# see Zivot_Andrews_1992_Test.R
n <- nrow(dat1)
dat1$overall.trend <- 1:n
# first break is a change in the trend in August 1998
dat1$break.trend.1998 <- 0
dat1$break.trend.1998[which(dat1$Date == "01.08.1998"):n] <- 0:(n - which(dat1$Date == "01.08.1998"))
# second break is a change in bot, the intercept and the slope and occurs in September 2014
dat1$break.intercept.2014 <- 0
dat1$break.intercept.2014[which(dat1$Date == "01.09.2014"):n] <- 1
dat1$break.trend.2014 <- 0
dat1$break.trend.2014[which(dat1$Date == "01.09.2014"):n] <- 0:(n - which(dat1$Date == "01.09.2014"))


# add macroeconomic uncertainty variable
# Reference: Jurado, K., S.C.Ludvigson, and S.NG (2015), "Measuring Uncertainty",
# American Economic Review, 105(3), 1177-1216.
dat1$macro.U.h1 <- dat1$macro.U.h1
dat1$dlmacro.U.h1 <- c(NA, diff(log(dat1$macro.U.h1)))
dat1$macro.U.h1.l1 <- c(NA, dat1$macro.U.h1[1:(n-1)])
dat1$macro.U.h3 <- dat1$macro.U.h3
dat1$macro.U.h12 <- dat1$macro.U.h12


# add risk premium
# Reference: Hamilton, J. and J.C. Wu (2014), "Risk Premia in Crude Oil Futures Prices",
# Journal of International Money and Finance, 42, 9-37.
dat.rp <- read.table("RP_all_new_2018.txt", header = TRUE)
dat.rp$date <- as.character(as.Date(as.character(dat.rp$date_weekly), "%Y%m%d"))
dat.rp_crude <- zoo(dat.rp$rp_new_2018, dat.rp$date)
firstofmonth <- function(x) as.Date(sub("..$", "01", format(x)))
dat.rp_crude.m <- aggregate(dat.rp_crude, firstofmonth(index(dat.rp_crude)), mean)
dat1$riskp_crude <- as.numeric(-dat.rp_crude.m[-1])
dat1$dlriskp_crude <- c(NA, diff(log(1 - dat1$riskp_crude)))
dat1$riskp.l1 <- c(NA, dat1$riskp_crude[-nrow(dat1)])
# -------------------------------------------------------------




# --------- regression on real oil prices ------------- -------------------
# regression on log real oil prices. Full sample
# we use logs here to improve interpretability of coefficients. The use of logs produces virtually
# the same results compared to using levels of the dependent variable
fm.price <- loil.spot.real ~ kilian + tb3m.real + dlus.inv + dlfx + vix + dlsp500 + macro.U.h1 + riskp_crude +
+ overall.trend + break.trend.1998 + break.intercept.2014 + break.trend.2014 
fit1 <- lm(fm.price, data = dat1)  
summary(fit1) 




# regression on log real oil prices. Pre financialization sample from 1990 until 2008
dat.pre <- dat1[1:which(dat1$Date == "01.09.2008"),]
fm.price <- loil.spot.real ~ kilian + tb3m.real + dlus.inv + dlfx + vix + dlsp500 + macro.U.h1 + riskp_crude +
+ overall.trend + break.trend.1998
fit1 <- lm(fm.price, data = dat.pre)  
summary(fit1) 


# regression on log real oil prices. Financialization sample after 2004 
dat.post <- dat1[which(dat1$Date == "01.01.2004"):n,]
fm.price <- loil.spot.real ~ kilian + tb3m.real + dlus.inv + dlfx + vix + dlsp500 + macro.U.h1 + riskp_crude +
+ overall.trend + break.intercept.2014 + break.trend.2014 
fit1 <- lm(fm.price, data = dat.post)  
summary(fit1) 



# Rolling Window base on the LMG measure from the realimpo package:
library(relaimpo)

dates <- as.character(dat1$Date)
# order names in fundamental and financial variables
names.table <- c("oil.spot.real", "overall.trend", "break.trend.1998", "break.intercept.2014", "break.trend.2014",
"kilian", "tb3m.real", "dlus.inv", "dlfx", "vix", "dlsp500", "macro.U.h1", "riskp_crude")
dat2 <- dat1[,which(names(dat1) %in% names.table)]

n<-nrow(dat2)  

s<-12*5 #window size  

coef.mat <- matrix(0,12,n-s) 
R.mat <- matrix(0,12,(n-s+1)) 
R.mat0 <- matrix(0,1,(n-s+1)) 

rownames(R.mat) <- names.table[-1]


for(i in 1:(n-s+1)) {  
# Achtung: formula should be the same as names.table above in order to keep correct variable ordering
fm.price <- oil.spot.real ~ overall.trend + break.trend.1998 + break.intercept.2014 + break.trend.2014 +
kilian + tb3m.real + dlus.inv + dlfx + vix + dlsp500 + macro.U.h1 + riskp_crude

tmp1 <- dat2[i:(i+s-1),]
# remove trend and intercept breaks from data set and from formula object in case they are all NA
if (all(tmp1$break.trend.1998 == 0)) {
tmp1 <- tmp1[,-which(names(tmp1) == "break.trend.1998")]
fm.tmp0 <- strsplit(as.character(fm.price)[3], split = "\\+")
fm.tmp1 <- fm.tmp0[[1]][-grep("break.trend.1998", fm.tmp0[[1]])]
fm.tmp2 <- paste(fm.tmp1, collapse = "+")
fm.price <- as.formula(paste(as.character(fm.price)[2], as.character(fm.price)[1], fm.tmp2))
}
if (all(tmp1$break.intercept.2014 == 0)) {
tmp1 <- tmp1[,-which(names(tmp1) == "break.intercept.2014")]
fm.tmp0 <- strsplit(as.character(fm.price)[3], split = "\\+")
fm.tmp1 <- fm.tmp0[[1]][-grep("break.intercept.2014", fm.tmp0[[1]])]
fm.tmp2 <- paste(fm.tmp1, collapse = "+")
fm.price <- as.formula(paste(as.character(fm.price)[2], as.character(fm.price)[1], fm.tmp2))
}
if (all(tmp1$break.trend.2014 == 0)) {
tmp1 <- tmp1[,-which(names(tmp1) == "break.trend.2014")]
fm.tmp0 <- strsplit(as.character(fm.price)[3], split = "\\+")
fm.tmp1 <- fm.tmp0[[1]][-grep("break.trend.2014", fm.tmp0[[1]])]
fm.tmp2 <- paste(fm.tmp1, collapse = "+")
fm.price <- as.formula(paste(as.character(fm.price)[2], as.character(fm.price)[1], fm.tmp2))
}
# over time, the overall trend and the break.trend.1998 become linear dependent which
# produces an error. We remove the trend 1998 when its correlation with the overall trend gets
# higher than 0.9
if (!all(tmp1$break.trend.1998 == 0)) {
if (cor(tmp1$overall.trend, tmp1$break.trend.1998) > 0.9) {
tmp1 <- tmp1[,-which(names(tmp1) == "break.trend.1998")]
fm.tmp0 <- strsplit(as.character(fm.price)[3], split = "\\+")
fm.tmp1 <- fm.tmp0[[1]][-grep("break.trend.1998", fm.tmp0[[1]])]
fm.tmp2 <- paste(fm.tmp1, collapse = "+")
fm.price <- as.formula(paste(as.character(fm.price)[2], as.character(fm.price)[1], fm.tmp2))
}
}
fit1 <- lm(fm.price, data=tmp1) 
R.mat0[,i] <- summary(fit1)$r.squared  
tmp2 <- lm(fm.price, data = tmp1)
tmp3 <- calc.relimp(tmp2, type = "lmg", rela = FALSE) # pmvd
for (j in 1:nrow(R.mat)) {
R.mat[which(rownames(R.mat) == tmp3@namen[-1][j]),i] <- tmp3$lmg[which(names(tmp3$lmg) == tmp3@namen[-1][j])]
}
}  


# LMG scales back to original R-squared:
all.equal(colSums(R.mat), R.mat0[1,])
 
dates <- as.character(dates)[s:n] 
n <- ncol(R.mat) 

#graph
plot(R.mat[1,], type = "n", ylim = c(0,1.27), xaxt = "n",
ylab = "Contribution to Total Variation in Crude Oil Prices",
xlab = "", cex.axis = 1.2, cex.lab = 1.2) 
# grid(10,10, col = grey(0.6)) 
axis(1, at = c(1,1/4*n,1/2*n,3/4*n,n),
labels = c(dates[1],dates[1/4*n],dates[1/2*n],dates[3/4*n],dates[n]), cex.axis = 1.2) 
xx <- c(1:n,n:1)

# plot trend and level shifts
tmp <- R.mat[1,] 
polygon(xx, c(rep(0,n),rev(tmp)), col = "khaki2", border = NA) # col = rgb(255/255,215/255,69/255, alpha = 0.4), border = NA) 
polygon(xx, c(tmp,rev(tmp + R.mat[2,])), col = "khaki3", border = NA) # col = rgb(255/255,215/255,69/255, alpha = 0.6), border = NA) 
tmp <- R.mat[1,] + R.mat[2,] 
polygon(xx, c(tmp,rev(tmp + R.mat[3,])), col = "gold", border = NA)
# density = 30, angle = 45, border = NA) # col = rgb(255/255,215/255,69/255, alpha = 0.8), border = NA) 
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] 
polygon(xx, c(tmp,rev(tmp + R.mat[4,])), col = "goldenrod2", border = NA)
# density = 30, angle = -45,border = NA)  # col = rgb(255/255,215/255,69/255, alpha = 1), border = NA) 

# plot fundamental variables
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,]+ R.mat[4,]
polygon(xx, c(tmp,rev(tmp + R.mat[5,])), col = "darkseagreen1", border = NA) # col = rgb(0/255,139/255,69/255, alpha = 0.4), border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]
polygon(xx, c(tmp,rev(tmp + R.mat[6,])), col = "lightgreen", border = NA) # col = rgb(0/255,139/255,69/255, alpha = 0.6), border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,]
polygon(xx, c(tmp,rev(tmp + R.mat[7,])), col = "green3", border = NA)
#density = 30, angle = 45, border = NA) #col = rgb(0/255,139/255,69/255, alpha = 0.8), border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,]
polygon(xx, c(tmp,rev(tmp + R.mat[8,])), col = "green4", border = NA)
#density = 30, angle = -45, border = NA) #col = rgb(0/255,139/255,69/255, alpha = 1), border = NA)

# plot financialization variables:
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,] + R.mat[8,]
polygon(xx, c(tmp,rev(tmp + R.mat[9,])), col = "lightpink1", border = NA) # col = rgb(139/255,0/255,0/255, alpha = 0.4), border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,] + R.mat[8,] + R.mat[9,]
polygon(xx, c(tmp,rev(tmp + R.mat[10,])), col = "lightpink3", border = NA) # col = rgb(139/255,0/255,0/255, alpha = 0.6), border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,] + R.mat[8,] + R.mat[9,] + R.mat[10,]
polygon(xx, c(tmp,rev(tmp + R.mat[11,])), col = "indianred3", border = NA)
#density = 30, angle = 45, border = NA) # col = rgb(139/255,0/255,0/255, alpha = 0.8), border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,] + R.mat[8,] + R.mat[9,] + R.mat[10,] + R.mat[11,]
polygon(xx, c(tmp,rev(tmp + R.mat[12,])), col = "darkred", border = NA)
# density = 30, angle = -45, border = NA) # col = rgb(139/255,0/255,0/255, alpha = 1), border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,] + R.mat[8,] + R.mat[9,] + R.mat[10,] + R.mat[11,] + R.mat[12,]
polygon(xx, c(tmp,rev(rep(1,n))), col = grey(0.9), border = NA) # col = rgb(238/255,233/255,233/255, alpha = 0.6), border = NA)
text(which(dates == "01.05.1997"), 0.96, "Unexplained Variation", cex = 1.2, col = grey(0.4))



# Lehman Brothers:
abline(v = which(dates == "01.09.2008"), lwd = 2, lty = 2, col = "steelblue4")
text(which(dates == "01.04.2006"), 1.02, "Lehman Brothers Default", cex = 1, col = "steelblue4", pos = 1)

# June 2014 Oil Price Drop:
abline(v = which(dates == "01.06.2014"), lwd = 2, lty = 2, col = "steelblue4")
text(which(dates == "01.01.2013"), 0.96, "June 2014 \n Oil Price Drop", cex = 1, col = "steelblue4")

# add some variable labels to improve readability:
text(which(dates == "01.02.2006"), 0.1, "Overall Trend", cex = 1)
text(which(dates == "01.02.2003"), 0.3, "Economic Activity", cex = 1)
text(which(dates == "01.07.2012"), 0.37, "VIX", cex = 1)

# add Financialization period labels:
text(which(dates == "01.02.2003"), 1.04, "Pre-Financialization", cex = 1.4)
text(which(dates == "01.07.2011"), 1.04, "Financialization", cex = 1.4)
text(which(dates == "01.10.2016"), 1.07, "De-Finan- \n cialization", cex = 1.4)



legend("top", legend = c("Overall Trend", "Trend Break 1998", "Intercept Break 2014",
"Trend Break 2014", "Econ. Activity","Interest Rate", expression(paste(Delta,"Inventory",sep = "")),
expression(paste(Delta,"FX")), "VIX", expression(paste(Delta,"S&P500")),"Macro Uncertainty","Risk Premium"), # names.table[-1], 
fill = c(
"khaki2","khaki3","gold","goldenrod2","darkseagreen1","lightgreen","green3","green4",
"lightpink1","lightpink3","indianred3","darkred"),
border = c(
"khaki2","khaki3","gold","goldenrod2","darkseagreen1","lightgreen","green3","green4",
"lightpink1","lightpink3","indianred3","darkred"),
inset = 0.01, bg = "white", ncol = 5)


# ---- pie graphs for overall contribution to R-squared pre and post financialization: -----
R.mat.pre <- R.mat[,1:165]
pre.trend <- mean(apply(R.mat.pre[which(row.names(R.mat.pre) %in% c("overall.trend","break.trend.1998",
"break.intercept.2014","break.trend.2014")),],2,sum))
pre.fundamental <- mean(apply(R.mat.pre[which(row.names(R.mat.pre) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
pre.financial <- mean(apply(R.mat.pre[which(row.names(R.mat.pre) %in% c("vix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
pre.unexplained <- 1 - pre.trend - pre.fundamental - pre.financial
pie(c(pre.fundamental,pre.financial,pre.trend,pre.unexplained),
col = c("green4","darkred","goldenrod2",grey(0.92)),
labels = c(paste("Fundamental \n", round(pre.fundamental*100,0),"%"),
paste("Financial \n", round(pre.financial*100,0),"%"),
paste("Breaks and Trends \n", round(pre.trend*100,0),"%"),
paste("Unexplained \n", round(pre.unexplained*100,0),"%")), cex = 1.2)


R.mat.post <- R.mat[,166:235]
post.trend <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("overall.trend","break.trend.1998",
"break.intercept.2014","break.trend.2014")),],2,sum))
post.fundamental <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
post.financial <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("vix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
post.unexplained <- 1 - post.trend - post.fundamental - post.financial
pie(c(post.fundamental,post.financial,post.trend,post.unexplained),
col = c("green4","darkred","goldenrod2",grey(0.92)),
labels = c(paste("Fundamental \n", round(post.fundamental*100,0),"%"),
paste("Financial \n", round(post.financial*100,0),"%"),
paste("Breaks and Trends \n", round(post.trend*100,0),"%"),
paste("Unexplained \n", round(post.unexplained*100,0),"%")), cex = 1.2)


R.mat.post <- R.mat[,236:290]
post.trend <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("overall.trend","break.trend.1998",
"break.intercept.2014","break.trend.2014")),],2,sum))
post.fundamental <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
post.financial <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("vix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
post.unexplained <- 1 - post.trend - post.fundamental - post.financial
pie(c(post.fundamental,post.financial,post.trend,post.unexplained), #edges = 100000,
col = c("green4","darkred","goldenrod2",grey(0.92)),
labels = c(paste("Fundamental \n", round(post.fundamental*100,0),"%"),
paste("Financial \n", round(post.financial*100,0),"%"),
paste("Breaks and Trends \n", round(post.trend*100,0),"%"),
paste("Unexplained \n", round(post.unexplained*100,0),"%")), cex = 1.2)
# --------------------------------------
# -------------------------------------------------------------------------




# ---------------------- regressions on oil price changes - --------
# Full sample
fm.ret <- dloil.spot ~ kilian + tb3m.real + dlus.inv + dlfx + vix + dlsp500 + macro.U.h1 + riskp_crude
fit1 <- lm(fm.ret, data = dat1)  
summary(fit1) 


# Pre financialization sample from 1990 until 2004
dat.pre <- dat1[1:which(dat1$Date == "01.12.2003"),]
fit1 <- lm(fm.ret, data = dat.pre)  
summary(fit1) 

# Financialization sample until after 2004 
dat.post <- dat1[which(dat1$Date == "01.01.2004"):n,]
fit1 <- lm(fm.ret, data = dat.post)  
summary(fit1) 


# Rolling Window base on the LMG measure from the realimpo package:
library(relaimpo)

dates <- as.character(dat1$Date)
# order names in fundamental and financial variables
names.table <- c("dloil.spot", "kilian", "tb3m.real", "dlus.inv", "dlfx", "dlvix", "dlsp500",
"macro.U.h1", "riskp_crude")
dat2 <- dat1[,which(names(dat1) %in% names.table)]

n<-nrow(dat2)  

s<-12*5 #window size. 5 years seems to be the best trade-off between noise and underlying trend.

coef.mat <- matrix(0,8,n-s) 
R.mat <- matrix(0,8,(n-s+1)) 
R.mat0 <- matrix(0,1,(n-s+1)) 

rownames(R.mat) <- names.table[-1]

# Achtung: formula should be the same as names.table above in order to keep correct variable ordering
fm.ret <- dloil.spot ~ kilian + tb3m.real + dlus.inv + dlfx + dlvix + dlsp500 + macro.U.h1 + riskp_crude

for(i in 1:(n-s+1)) {  
tmp1<-dat2[i:(i+s-1),] 
fit1 <- lm(fm.ret, data=tmp1) 
R.mat0[,i] <- summary(fit1)$r.squared  
tmp2 <- lm(fm.ret, data = tmp1)
tmp3 <- calc.relimp(tmp2, type = "lmg", rela = FALSE) # pmvd
R.mat[,i] <- tmp3$lmg # tmp3$pmvd
}  

# LMG scales back to original R-squared:
all.equal(colSums(R.mat), R.mat0[1,])
 
dates <- as.character(dates)[s:n] 
n <- ncol(R.mat) 

#graph
plot(R.mat[1,], type = "n", ylim = c(0,1.15), xaxt = "n",
ylab = "Contribution to Total Variation in Crude Oil Returns",
xlab = "", cex.axis = 1.2, cex.lab = 1.2) 
axis(1, at = c(1,1/4*n,1/2*n,3/4*n,n),
labels = c(dates[1],dates[1/4*n],dates[1/2*n],dates[3/4*n],dates[n]), cex.axis = 1.2) 
xx <- c(1:n,n:1) 

# plot fundamental variables
tmp <- R.mat[1,]
polygon(xx, c(rep(0,n),rev(tmp)),  col = "darkseagreen1", border = NA)
polygon(xx, c(tmp,rev(tmp + R.mat[2,])), col = "lightgreen", border = NA)
tmp <- R.mat[1,] + R.mat[2,] 
polygon(xx, c(tmp,rev(tmp + R.mat[3,])), col = "green3", border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] 
polygon(xx, c(tmp,rev(tmp + R.mat[4,])), col = "green4", border = NA)

# plot financialization variables:
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,]+ R.mat[4,]
polygon(xx, c(tmp,rev(tmp + R.mat[5,])), col = "lightpink1", border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]
polygon(xx, c(tmp,rev(tmp + R.mat[6,])), col = "lightpink3", border = NA) 
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,]
polygon(xx, c(tmp,rev(tmp + R.mat[7,])), col = "indianred3", border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,]
polygon(xx, c(tmp,rev(tmp + R.mat[8,])), col = "darkred", border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,] + R.mat[8,]
polygon(xx, c(tmp,rev(rep(1,n))), col = grey(0.92), border = NA) 
text(which(dates == "01.10.1997"), 0.96, "Unexplained Variation", cex = 1.2, col = grey(0.4))

# Lehman Brothers:
abline(v = which(dates == "01.09.2008"), lwd = 2, lty = 2, col = "steelblue4")
text(which(dates == "01.01.2006"), 1.01, "Lehman Brothers Default", cex = 1, col = "steelblue4", pos = 1)

# June 2014 Oil Price Drop:
abline(v = which(dates == "01.06.2014"), lwd = 2, lty = 2, col = "steelblue4")
text(which(dates == "01.11.2012"), 0.96, "June 2014 \n Oil Price Drop", cex = 1, col = "steelblue4")

# add some variable labels to improve readability:
text(which(dates == "01.02.2011"), 0.05, "Economic Activity", cex = 1)
text(which(dates == "01.06.2010"), 0.24, expression(paste(Delta,"FX")), cex = 1)
text(which(dates == "01.04.2014"), 0.18, expression(paste(Delta,"VIX")), cex = 1)
text(which(dates == "01.05.2012"), 0.45, expression(paste(Delta,"S&P500")), cex = 1)

# add Financialization period labels:
text(which(dates == "01.02.2003"), 0.85, "Pre-Financialization", cex = 1.4)
text(which(dates == "01.07.2011"), 0.85, "Financialization", cex = 1.4)
text(which(dates == "01.02.2017"), 0.85, "De-Finan- \n cialization", cex = 1.4)


legend("top", legend = c("Econ. Activity","Interest Rate", expression(paste(Delta,"Inventory",sep = "")),
expression(paste(Delta,"FX")), expression(paste(Delta,"VIX")), expression(paste(Delta,"S&P500")),"Macro Uncertainty","Risk Premium"), # names.table[-1], 
fill = c(
"darkseagreen1","lightgreen","green3","green4",
"lightpink1","lightpink3","indianred3","darkred"),
border = c(
"darkseagreen1","lightgreen","green3","green4",
"lightpink1","lightpink3","indianred3","darkred"),
inset = 0.02, bg = "white", ncol = 5)


# ---- pie graphs for overall contribution to R-squared pre, during, and De-financialization: -----
n <- ncol(R.mat) 
R.mat.pre <- R.mat[,1:165] # ends in August 2008: > dates[165] "01.08.2008"
pre.fundamental <- mean(apply(R.mat.pre[which(row.names(R.mat.pre) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
pre.financial <- mean(apply(R.mat.pre[which(row.names(R.mat.pre) %in% c("dlvix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
pre.unexplained <- 1 - pre.fundamental - pre.financial
pie(c(pre.fundamental,pre.financial,pre.unexplained),
col = c("green4","darkred",grey(0.92)),
labels = c(paste("Fundamental \n", round(pre.fundamental*100,0),"%"),
paste("Financial \n", round(pre.financial*100,0),"%"),
paste("Unexplained \n", round(pre.unexplained*100,0),"%")), cex = 1.2)

R.mat.post <- R.mat[,166:235] # starts after Lehman Brothers and goes until June 2014: dates[235] "01.06.2014"
# how much do economic activity and delta FX explain on average? (see paper)
mean(R.mat.post["kilian",])
mean(R.mat.post["dlfx",])
mean(R.mat.post["dlvix",])
mean(R.mat.post["dlsp500",])

# how much do economic activity and delta FX explain on average? (see paper)
mean(R.mat.post["dlvix",]) + mean(R.mat.post["dlsp500",])
post.fundamental <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
post.financial <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("dlvix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
post.unexplained <- 1 - post.fundamental - post.financial
pie(c(post.fundamental,post.financial,post.unexplained),
col = c("green4","darkred",grey(0.92)),
labels = c(paste("Fundamental \n", round(post.fundamental*100,0),"%"),
paste("Financial \n", round(post.financial*100,0),"%"),
paste("Unexplained \n", round(post.unexplained*100,0),"%")), cex = 1.2)

R.mat.post <- R.mat[,236:290] # starts after Lehman Brothers and goes until June 2014: dates[235] "01.06.2014"
# how much do economic activity and delta FX explain on average? (see paper)
mean(R.mat.post["kilian",])
mean(R.mat.post["dlfx",])
# how much do economic activity and delta FX explain on average? (see paper)
mean(R.mat.post["dlvix",]) + mean(R.mat.post["dlsp500",])
post.fundamental <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
post.financial <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("dlvix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
post.unexplained <- 1 - post.fundamental - post.financial
pie(c(post.fundamental,post.financial,post.unexplained),
col = c("green4","darkred",grey(0.92)),
labels = c(paste("Fundamental \n", round(post.fundamental*100,0),"%"),
paste("Financial \n", round(post.financial*100,0),"%"),
paste("Unexplained \n", round(post.unexplained*100,0),"%")), cex = 1.2)
# --------------------------------------
# -------------------------------------------------------------------------



# ------------------ regressions on oil volatility --------------- --------
# Full sample
fm <- oil.spot.vol ~ kilian + tb3m.real + dlus.inv + dlfx + vix + dlsp500 + macro.U.h1 + riskp_crude
fit1 <- lm(fm, data = dat1)  
summary(fit1) 

# Pre financialization sample from 1990 until 2004
dat.pre <- dat1[1:which(dat1$Date == "01.09.2004"),]
fit1 <- lm(fm, data = dat.pre)  
summary(fit1) 

# Financialization sample until after 2004 
dat.post <- dat1[which(dat1$Date == "01.02.2004"):n,]
fit1 <- lm(fm, data = dat.post)  
summary(fit1) 


# why we need the VIX in addition to the macroeconomic uncertainty index:
# It covers time-varying risk-aversion which is important for the behavior of financial investors and 
# their risk bearing capabilities. It is not the same as economic uncertainty


#Newey West adjustment for 5 lags
coeftest(fit1, NeweyWest(fit1))



# Rolling Window base on the LMG measure from the realimpo package:
library(relaimpo)

dates <- as.character(dat1$Date)
# order names in fundamental and financial variables
names.table <- c("oil.spot.vol","kilian","tb3m.real","dlus.inv","dlfx","vix","dlsp500","macro.U.h1","riskp_crude")
dat2 <- dat1[,which(names(dat1) %in% names.table)]

n<-nrow(dat2)  

s<-12*5 #window size. Try 9 years, yields interesting results

coef.mat <- matrix(0,8,n-s) 
R.mat <- matrix(0,8,(n-s+1)) 
R.mat0 <- matrix(0,1,(n-s+1)) 

rownames(R.mat) <- names.table[-1]

# Achtung: formula should be the same as names.table above in order to keep correct variable ordering
fm.vol <- oil.spot.vol ~ kilian + tb3m.real + dlus.inv + dlfx + vix  + dlsp500 + macro.U.h1 + riskp_crude

for(i in 1:(n-s+1)) {  
tmp1<-dat2[i:(i+s-1),] 
fit1 <- lm(fm.vol, data=tmp1) 
R.mat0[,i] <- summary(fit1)$r.squared  
tmp2 <- lm(fm.vol, data = tmp1)
tmp3 <- calc.relimp(tmp2, type = "lmg", rela = FALSE) # pmvd
R.mat[,i] <- tmp3$lmg # tmp3$pmvd
}  

# LMG scales back to original R-squared:
all.equal(colSums(R.mat), R.mat0[1,])
 
dates <- as.character(dates)[s:n] 
n <- ncol(R.mat) 

#graph
plot(R.mat[1,], type = "n", ylim = c(0,1.15), xaxt = "n",
ylab = "Contribution to Total Variation in Crude Oil Volatility",
xlab = "", cex.axis = 1.2, cex.lab = 1.2) 
axis(1, at = c(1,1/4*n,1/2*n,3/4*n,n),
labels = c(dates[1],dates[1/4*n],dates[1/2*n],dates[3/4*n],dates[n]), cex.axis = 1.2) 
xx <- c(1:n,n:1) 

# plot fundamental variables
tmp <- R.mat[1,]
polygon(xx, c(rep(0,n),rev(tmp)),  col = "darkseagreen1", border = NA)
polygon(xx, c(tmp,rev(tmp + R.mat[2,])), col = "lightgreen", border = NA)
tmp <- R.mat[1,] + R.mat[2,] 
polygon(xx, c(tmp,rev(tmp + R.mat[3,])), col = "green3", border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] 
polygon(xx, c(tmp,rev(tmp + R.mat[4,])), col = "green4", border = NA)

# plot financialization variables:
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,]+ R.mat[4,]
polygon(xx, c(tmp,rev(tmp + R.mat[5,])), col = "lightpink1", border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]
polygon(xx, c(tmp,rev(tmp + R.mat[6,])), col = "lightpink3", border = NA) 
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,]
polygon(xx, c(tmp,rev(tmp + R.mat[7,])), col = "indianred3", border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,]
polygon(xx, c(tmp,rev(tmp + R.mat[8,])), col = "darkred", border = NA)
tmp <- R.mat[1,] + R.mat[2,] + R.mat[3,] + R.mat[4,] + R.mat[5,]+ R.mat[6,] + R.mat[7,] + R.mat[8,]
polygon(xx, c(tmp,rev(rep(1,n))), col = grey(0.92), border = NA) 
text(which(dates == "01.10.1997"), 0.96, "Unexplained Variation", cex = 1.2, col = grey(0.4))

# Lehman Brothers:
abline(v = which(dates == "01.09.2008"), lwd = 2, lty = 2, col = "steelblue4")
text(which(dates == "01.01.2006"), 1.01, "Lehman Brothers Default", cex = 1, col = "steelblue4", pos = 1)

# June 2014 Oil Price Drop:
abline(v = which(dates == "01.06.2014"), lwd = 2, lty = 2, col = "steelblue4")
text(which(dates == "01.11.2012"), 0.96, "June 2014 \n Oil Price Drop", cex = 1, col = "steelblue4")

# add some variable labels to improve readability:
text(which(dates == "01.06.2010"), 0.04, "Economic Activity", cex = 1)
text(which(dates == "01.06.2013"), 0.16, "Interest \n Rate", cex = 1)
text(which(dates == "01.11.2011"), 0.3, "VIX", cex = 1)
text(which(dates == "01.02.2012"), 0.57, "Macro Uncertainty", cex = 1)
text(which(dates == "01.01.2012"), 0.775, "Risk Premium", cex = 1)

# add Financialization period labels:
text(which(dates == "01.02.2003"), 0.88, "Pre-Financialization", cex = 1.4)
text(which(dates == "01.07.2011"), 0.88, "Financialization", cex = 1.4)
text(which(dates == "01.02.2017"), 0.85, "De-Finan- \n cialization", cex = 1.4)


legend("top", legend = c("Econ. Activity","Interest Rate", expression(paste(Delta,"Inventory",sep = "")),
expression(paste(Delta,"FX")), "VIX", expression(paste(Delta,"S&P500")),"Macro Uncertainty","Risk Premium"), # names.table[-1], 
fill = c(
"darkseagreen1","lightgreen","green3","green4",
"lightpink1","lightpink3","indianred3","darkred"),
border = c(
"darkseagreen1","lightgreen","green3","green4",
"lightpink1","lightpink3","indianred3","darkred"),
inset = 0.02, bg = "white", ncol = 5)


# ---- pie graphs for overall contribution to R-squared pre and post financialization: -----
n<-nrow(dat2)  
R.mat.pre <- R.mat[,1:165] # pre ends just before Sep.2008, see dates[165]
pre.fundamental <- mean(apply(R.mat.pre[which(row.names(R.mat.pre) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
pre.financial <- mean(apply(R.mat.pre[which(row.names(R.mat.pre) %in% c("vix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
pre.unexplained <- 1 - pre.fundamental - pre.financial
pie(c(pre.fundamental,pre.financial,pre.unexplained),
col = c("green4","darkred",grey(0.92)),
labels = c(paste("Fundamental \n", round(pre.fundamental*100,0),"%"),
paste("Financial \n", round(pre.financial*100,0),"%"),
paste("Unexplained \n", round(pre.unexplained*100,0),"%")), cex = 1.2)

R.mat.post <- R.mat[,166:235] # post is after Lehman Brothers, see dates[235]
post.fundamental <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
post.financial <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("vix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
post.unexplained <- 1 - post.fundamental - post.financial
pie(c(post.fundamental,post.financial,post.unexplained),
col = c("green4","darkred",grey(0.92)),
labels = c(paste("Fundamental \n", round(post.fundamental*100,0),"%"),
paste("Financial \n", round(post.financial*100,0),"%"),
paste("Unexplained \n", round(post.unexplained*100,0),"%")), cex = 1.2)

R.mat.post <- R.mat[,236:290] # post is after Lehman Brothers.
post.fundamental <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("kilian","tb3m.real",
"dlus.inv","dlfx" )),],2,sum))
post.financial <- mean(apply(R.mat.post[which(row.names(R.mat.post) %in% c("vix","dlsp500",
"macro.U.h1","riskp_crude" )),],2,sum))
post.unexplained <- 1 - post.fundamental - post.financial
pie(c(post.fundamental,post.financial,post.unexplained),
col = c("green4","darkred",grey(0.92)),
labels = c(paste("Fundamental \n", round(post.fundamental*100,0),"%"),
paste("Financial \n", round(post.financial*100,0),"%"),
paste("Unexplained \n", round(post.unexplained*100,0),"%")), cex = 1.2)


# --------------------------------------
# -------------------------------------------------------------------------


