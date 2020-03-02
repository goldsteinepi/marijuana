################################################################################
#   Interrupted time series regression for the evaluation of arrests related 
#     marijuana possession 
#   N. Tran; N. Goldstein; J. Purtle; P. Massey; S. Lankenau; J Suder; L. Tabb
################################################################################

#load packages
library(tsModel); library(Epi); library(scales)

# read data from csv file
setwd("")
data = read.csv("~/marijuana_v1/Final Resubmission/MarijuanaArrest.csv")
data.race = read.csv("~/marijuana_v1/Final Resubmission/MarijuanaArrestRace.csv")
data.sex = read.csv("~/marijuana_v1/Final Resubmission/MarijuanaArrestSex.csv")
data.age = read.csv("~/marijuana_v1/Final Resubmission/MarijuanaArrestAge.csv")

# compute the arrest rates
# data$rate.tot <- with(data,arrest.tot/pop * 10^5)
# data$rate.pos <- with(data,arrest.pos/pop * 10^5)
# data$rate.man <- with(data,arrest.man/pop * 10^5)
# data$rate.police <- with(data,officer/totpop * 10^5)

# decriminalization  
data$decrim <- as.factor(ifelse(data$time >= 66, 1, 0))

# include 22-month lag for medical marijuana legalization 
data$medmj_lag <- as.factor(ifelse(data$time >= 110, 1, 0))

# exposure group: 1=philly, 0=pittsburgh
data$expose <- as.factor(data$expose)

# Step 1: Descriptive analyses ####
# tabulate arrest rates before and after the decriminalization 
# philadelphia, pre-decriminalization 
mean(data[data$expose==1 & data$decrim==0,10]); sd(data[data$expose==1 & data$decrim==0,10]) 
mean(data[data$expose==1 & data$decrim==0,11]); sd(data[data$expose==1 & data$decrim==0,11])
mean(data[data$expose==1 & data$decrim==0,12]); sd(data[data$expose==1 & data$decrim==0,12])

# dauphin, pre-decriminalization
mean(data[data$expose==0 & data$decrim==0,10]); sd(data[data$expose==0 & data$decrim==0,10]) 
mean(data[data$expose==0 & data$decrim==0,11]); sd(data[data$expose==0 & data$decrim==0,11])
mean(data[data$expose==0 & data$decrim==0,12]); sd(data[data$expose==0 & data$decrim==0,12])

# philadelphia, post-decriminalization 
mean(data[data$expose==1 & data$decrim==1,10]); sd(data[data$expose==1 & data$decrim==1,10]) 
mean(data[data$expose==1 & data$decrim==1,11]); sd(data[data$expose==1 & data$decrim==1,11])
mean(data[data$expose==1 & data$decrim==1,12]); sd(data[data$expose==1 & data$decrim==1,12])

# dauphin, post-decriminalization
mean(data[data$expose==0 & data$decrim==1,10]); sd(data[data$expose==0 & data$decrim==1,10]) 
mean(data[data$expose==0 & data$decrim==1,11]); sd(data[data$expose==0 & data$decrim==1,11])
mean(data[data$expose==0 & data$decrim==1,12]); sd(data[data$expose==0 & data$decrim==1,12])

# Plot arrests rates by type of marijuana arrests 
jpeg("~/marijuana_v1/Final Resubmission/DAD_Fig1_final.jpeg",height=2*5*600,width=2*5*2100,res=1200)
par(mfrow=c(1,3))
plot(data[c(1:120),10],type="n",xaxt="n",ylab="Arrest Rates per 100 000", xlab="Years", ylim=c(0,55),xaxs="i", cex.lab=1.5, cex.axis=1.5)
axis(1,at=0:10*12,labels=F, cex.axis=1.5)
axis(1,at=0:9*12+12,tick=F,labels=2009:2018, cex.axis=1.5)
abline(v=65,lwd=1,lty=3)
abline(v=70,lwd=1,lty=3)
lines(data[c(121:185),10], lwd=1,lty=1, col = alpha("grey",1)) # allegheny 
points(x=data[c(186:189),3],y=data[c(186:189),10], pch=16, col = "grey")
lines(x=data[c(190:240),3],y=data[c(190:240),10], col= "grey")
lines(data[c(1:65),10], lwd=1,lty=1, col = alpha("black",1)) # philadelphia 
points(x=data[c(66:69),3],y=data[c(66:69),10], pch=16)
lines(x=data[c(70:120),3],y=data[c(70:120),10])
# legend("topright",inset = 0.01, c("Philadelphia","Dauphin"),lty=c(1,1),lwd=c(1,1),
# bty="n",cex=1, col=c("black","grey","grey","grey"))
title(main = "A. Overall Marijuana Related Arrests", adj = 0, cex.main=2)

plot(data[c(1:120),11],type="n",xaxt="n",ylab="Arrest Rates per 100 000", xlab="Years", ylim=c(0,55),xaxs="i",cex.lab=1.5, cex.axis=1.5)
axis(1,at=0:10*12,labels=F,cex.axis=1.5)
axis(1,at=0:9*12+12,tick=F,labels=2009:2018,cex.axis=1.5)
abline(v=65,lwd=1,lty=3)
abline(v=70,lwd=1,lty=3)
lines(data[c(121:185),11], lwd=1,lty=1, col = alpha("grey",1)) # allegheny 
points(x=data[c(186:189),3],y=data[c(186:189),11], pch=16, col = "grey")
lines(x=data[c(190:240),3],y=data[c(190:240),11], col= "grey")
lines(data[c(1:65),11], lwd=1,lty=1, col = alpha("black",1)) # philadelphia 
points(x=data[c(66:69),3],y=data[c(66:69),11], pch=16)
lines(x=data[c(70:120),3],y=data[c(70:120),11])
# legend("topright",inset = 0.01, c("Philadelphia","Dauphin"),lty=c(1,1),lwd=c(1,1),
# bty="n",cex=1, col=c("black","grey","grey","grey"))
title(main = "B. Arrests Related to Possession", adj = 0, cex.main = 2)

plot(data[c(1:120),12],type="n",xaxt="n",ylab="Arrest Rates per 100 000", xlab="Years", ylim=c(0,55),xaxs="i",cex.lab=1.5, cex.axis=1.5)
axis(1,at=0:10*12,labels=F,cex.axis=1.5)
axis(1,at=0:9*12+12,tick=F,labels=2009:2018,cex.axis=1.5)
abline(v=65,lwd=1,lty=3)
abline(v=70,lwd=1,lty=3)
lines(data[c(121:185),12], lwd=1,lty=1, col = alpha("grey",1)) # allegheny 
points(x=data[c(186:189),3],y=data[c(186:189),12], pch=16, col = "grey")
lines(x=data[c(190:240),3],y=data[c(190:240),12], col= "grey")
lines(data[c(1:65),12], lwd=1,lty=1, col = alpha("black",1)) # philadelphia
points(x=data[c(66:69),3],y=data[c(66:69),12], pch=16)
lines(x=data[c(70:120),3],y=data[c(70:120),12])
# legend("topright",inset = 0.01, c("Philadelphia","Dauphin"),lty=c(1,1),lwd=c(1,1),
# bty="n",cex=1, col=c("black","grey","grey","grey"))
title(main = "C. Arrests Related to Sales/Manufacturing", adj = 0, cex.main = 2)
dev.off()

# Step 2: Assessment of Parallel Trend Assumption ####
m1 = lm(rate.tot ~ expose*time + harmonic(month,2,12) + rate.police, data[data$decrim==0,])
m2 = lm(rate.pos ~ expose*time + harmonic(month,2,12) + rate.police, data[data$decrim==0,])
m3 = lm(rate.man ~ expose*time + harmonic(month,2,12) + rate.police, data[data$decrim==0,])

summary(m1); summary(m2); summary(m3)
round(ci.lin(m1,Exp=F),3); round(ci.lin(m2,Exp=F),3); round(ci.lin(m3,Exp=F),3)

rm(m1,m2,m3)

# Step 3: ITS models of all aggregated arrest data ####
# philadelphia
m1 = lm(rate.tot ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data[data$expose==1 & (data$time <= 65 | data$time >=70),])
m2 = lm(rate.pos ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data[data$expose==1 & (data$time <= 65 | data$time >=70),])
m3 = lm(rate.man ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data[data$expose==1 & (data$time <= 65 | data$time >=70),])

summary(m1); summary(m2); summary(m3)
round(ci.lin(m1,Exp=F),3); round(ci.lin(m2,Exp=F),3); round(ci.lin(m3,Exp=F),3)

# Bootstrapping
data1 = data[data$expose==1 & (data$time <= 65 | data$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m3abs <- lm(rate.man ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m1)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.025, 0.975)))
c(summary(m2)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.025, 0.975)))
c(summary(m3)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.025, 0.975)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

# dauphin
m4 = lm(rate.tot ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data[data$expose==0 & (data$time <= 65 | data$time >=70),])
m5 = lm(rate.pos ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data[data$expose==0 & (data$time <= 65 | data$time >=70),])
m6 = lm(rate.man ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data[data$expose==0 & (data$time <= 65 | data$time >=70),])

summary(m4); summary(m5); summary(m6)
round(ci.lin(m4,Exp=T),3); round(ci.lin(m5,Exp=T),3); round(ci.lin(m6,Exp=T),3)

# Bootstrapping
data1 = data[data$expose==0 & (data$time <= 65 | data$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m3abs <- lm(rate.man ~ decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m4)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.025, 0.975)))
c(summary(m5)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.025, 0.975)))
c(summary(m6)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.025, 0.975)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)

# CITS models 
m1 = lm(rate.tot ~ expose*decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m2 = lm(rate.pos ~ expose*decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m3 = lm(rate.man ~ expose*decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])

summary(m1); summary(m2); summary(m3)
round(ci.lin(m1,Exp=F),3); round(ci.lin(m2,Exp=F),3); round(ci.lin(m3,Exp=F),3)

# Model diagnostics 
acf(residuals(m1,type="deviance"))
pacf(residuals(m1,type="deviance"))
acf(residuals(m2,type="deviance"))
pacf(residuals(m2,type="deviance"))
acf(residuals(m3,type="deviance"))
pacf(residuals(m3,type="deviance"))

plot(m1); plot(m2); plot(m3)

# % change 
summary(m1)$coefficient[11,1] / (summary(m1)$coefficient[1,1] + summary(m1)$coefficient[2,1]) * 100
summary(m2)$coefficient[11,1] / (summary(m2)$coefficient[1,1] + summary(m2)$coefficient[2,1]) * 100
summary(m3)$coefficient[11,1] / (summary(m3)$coefficient[1,1] + summary(m3)$coefficient[2,1]) * 100

# Bootstrapping
data1 = data[(data$time <= 65 | data$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ expose*decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m2abs <- lm(rate.pos ~ expose*decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m3abs <- lm(rate.man ~ expose*decrim + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m1)$coefficients[11,1], quantile(bsdat_1a[,11], c(0.025, 0.975)))
c(summary(m2)$coefficients[11,1], quantile(bsdat_2a[,11], c(0.025, 0.975)))
c(summary(m3)$coefficients[11,1], quantile(bsdat_3a[,11], c(0.025, 0.975)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)



# step 4. Stratified Models by Race ####
data.race$race = as.factor(data.race$race)
data.race$decrim = as.factor(data.race$decrim)
data.race$expose = as.factor(data.race$expose)

# ITS for Philadelphia
m1 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & data.race$expose==1 & (data.race$time <= 65 | data.race$time >=70),])
m2 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & data.race$expose==1 & (data.race$time <= 65 | data.race$time >=70),])
m3 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & data.race$expose==1 & (data.race$time <= 65 | data.race$time >=70),])
m4 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & data.race$expose==1 & (data.race$time <= 65 | data.race$time >=70),])
m5 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & data.race$expose==1 & (data.race$time <= 65 | data.race$time >=70),])
m6 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & data.race$expose==1 & (data.race$time <= 65 | data.race$time >=70),])

data1 = data.race[data.race$race==0 & data.race$expose==1 & (data.race$time <= 65 | data.race$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m1)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

data1 = data.race[data.race$race==1 & data.race$expose==1 & (data.race$time <= 65 | data.race$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m4)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)

# ITS for Dauphin 
m1 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & data.race$expose==0 & (data.race$time <= 65 | data.race$time >=70),])
m2 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & data.race$expose==0 & (data.race$time <= 65 | data.race$time >=70),])
m3 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & data.race$expose==0 & (data.race$time <= 65 | data.race$time >=70),])
m4 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & data.race$expose==0 & (data.race$time <= 65 | data.race$time >=70),])
m5 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & data.race$expose==0 & (data.race$time <= 65 | data.race$time >=70),])
m6 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & data.race$expose==0 & (data.race$time <= 65 | data.race$time >=70),])

data1 = data.race[data.race$race==0 & data.race$expose==0 & (data.race$time <= 65 | data.race$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m1)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

data1 = data.race[data.race$race==1 & data.race$expose==0 & (data.race$time <= 65 | data.race$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m4)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)


# DID Regression
m1 = lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & (data.race$time <= 65 | data.race$time >=70),])
m2 = lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & (data.race$time <= 65 | data.race$time >=70),])
m3 = lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==0 & (data.race$time <= 65 | data.race$time >=70),])
m4 = lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & (data.race$time <= 65 | data.race$time >=70),])
m5 = lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & (data.race$time <= 65 | data.race$time >=70),])
m6 = lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.race[data.race$race==1 & (data.race$time <= 65 | data.race$time >=70),])


acf(residuals(m1,type="deviance"))
pacf(residuals(m1,type="deviance"))
acf(residuals(m2,type="deviance"))
pacf(residuals(m2,type="deviance"))
acf(residuals(m3,type="deviance"))
pacf(residuals(m3,type="deviance"))
acf(residuals(m4,type="deviance"))
pacf(residuals(m4,type="deviance"))
acf(residuals(m5,type="deviance"))
pacf(residuals(m5,type="deviance"))
acf(residuals(m6,type="deviance"))
pacf(residuals(m6,type="deviance"))

summary(m1); summary(m2); summary(m3);summary(m4); summary(m5); summary(m6);
round(ci.lin(m1,Exp=F),3); round(ci.lin(m2,Exp=F),3); round(ci.lin(m3,Exp=F),3); 
round(ci.lin(m4,Exp=F),3); round(ci.lin(m5,Exp=F),3); round(ci.lin(m6,Exp=F),3)

# Calculate percentage change 
summary(m1)$coefficient[11,1] / (summary(m1)$coefficient[1,1] + summary(m1)$coefficient[2,1]) * 100
summary(m2)$coefficient[11,1] / (summary(m2)$coefficient[1,1] + summary(m2)$coefficient[2,1]) * 100
summary(m3)$coefficient[11,1] / (summary(m3)$coefficient[1,1] + summary(m3)$coefficient[2,1]) * 100
summary(m4)$coefficient[11,1] / (summary(m4)$coefficient[1,1] + summary(m4)$coefficient[2,1]) * 100
summary(m5)$coefficient[11,1] / (summary(m5)$coefficient[1,1] + summary(m5)$coefficient[2,1]) * 100
summary(m6)$coefficient[11,1] / (summary(m6)$coefficient[1,1] + summary(m6)$coefficient[2,1]) * 100

data1 = data.race[data.race$race==0 & (data.race$time <= 65 | data.race$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m1)$coefficients[11,1], quantile(bsdat_1a[,11], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[11,1], quantile(bsdat_2a[,11], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[11,1], quantile(bsdat_3a[,11], c(0.002777778, 0.9972222)))

data1 = data.race[data.race$race==1 & (data.race$time <= 65 | data.race$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m4)$coefficients[11,1], quantile(bsdat_1a[,11], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[11,1], quantile(bsdat_2a[,11], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[11,1], quantile(bsdat_3a[,11], c(0.002777778, 0.9972222)))


# Step 5. Stratified Models by sex ####
data.sex$sex = as.factor(data.sex$sex)
data.sex$decrim = as.factor(data.sex$decrim)
data.sex$expose = as.factor(data.sex$expose)

# ITS for Philadelphia
m1 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & data.sex$expose==1 & (data.sex$time <= 65 | data.sex$time >=70),])
m2 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & data.sex$expose==1 & (data.sex$time <= 65 | data.sex$time >=70),])
m3 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & data.sex$expose==1 & (data.sex$time <= 65 | data.sex$time >=70),])
m4 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & data.sex$expose==1 & (data.sex$time <= 65 | data.sex$time >=70),])
m5 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & data.sex$expose==1 & (data.sex$time <= 65 | data.sex$time >=70),])
m6 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & data.sex$expose==1 & (data.sex$time <= 65 | data.sex$time >=70),])

data1 = data.sex[data.sex$sex==0 & data.sex$expose==1 & (data.sex$time <= 65 | data.sex$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m1)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

data1 = data.sex[data.sex$sex==1 & data.sex$expose==1 & (data.sex$time <= 65 | data.sex$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m4)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)

# ITS for Dauphin 
m1 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & data.sex$expose==0 & (data.sex$time <= 65 | data.sex$time >=70),])
m2 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & data.sex$expose==0 & (data.sex$time <= 65 | data.sex$time >=70),])
m3 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & data.sex$expose==0 & (data.sex$time <= 65 | data.sex$time >=70),])
m4 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & data.sex$expose==0 & (data.sex$time <= 65 | data.sex$time >=70),])
m5 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & data.sex$expose==0 & (data.sex$time <= 65 | data.sex$time >=70),])
m6 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & data.sex$expose==0 & (data.sex$time <= 65 | data.sex$time >=70),])

data1 = data.sex[data.sex$sex==0 & data.sex$expose==0 & (data.sex$time <= 65 | data.sex$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m1)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

data1 = data.sex[data.sex$sex==1 & data.sex$expose==0 & (data.sex$time <= 65 | data.sex$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m4)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)


# DID Regression
m1 = lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & (data.sex$time <= 65 | data.sex$time >=70),])
m2 = lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & (data.sex$time <= 65 | data.sex$time >=70),])
m3 = lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==0 & (data.sex$time <= 65 | data.sex$time >=70),])
m4 = lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & (data.sex$time <= 65 | data.sex$time >=70),])
m5 = lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & (data.sex$time <= 65 | data.sex$time >=70),])
m6 = lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.sex[data.sex$sex==1 & (data.sex$time <= 65 | data.sex$time >=70),])


acf(residuals(m1,type="deviance"))
pacf(residuals(m1,type="deviance"))
acf(residuals(m2,type="deviance"))
pacf(residuals(m2,type="deviance"))
acf(residuals(m3,type="deviance"))
pacf(residuals(m3,type="deviance"))
acf(residuals(m4,type="deviance"))
pacf(residuals(m4,type="deviance"))
acf(residuals(m5,type="deviance"))
pacf(residuals(m5,type="deviance"))
acf(residuals(m6,type="deviance"))
pacf(residuals(m6,type="deviance"))

summary(m1); summary(m2); summary(m3);summary(m4); summary(m5); summary(m6);
round(ci.lin(m1,Exp=F),3); round(ci.lin(m2,Exp=F),3); round(ci.lin(m3,Exp=F),3); 
round(ci.lin(m4,Exp=F),3); round(ci.lin(m5,Exp=F),3); round(ci.lin(m6,Exp=F),3)

# Calculate percentage change 
summary(m1)$coefficient[11,1] / (summary(m1)$coefficient[1,1] + summary(m1)$coefficient[2,1]) * 100
summary(m2)$coefficient[11,1] / (summary(m2)$coefficient[1,1] + summary(m2)$coefficient[2,1]) * 100
summary(m3)$coefficient[11,1] / (summary(m3)$coefficient[1,1] + summary(m3)$coefficient[2,1]) * 100
summary(m4)$coefficient[11,1] / (summary(m4)$coefficient[1,1] + summary(m4)$coefficient[2,1]) * 100
summary(m5)$coefficient[11,1] / (summary(m5)$coefficient[1,1] + summary(m5)$coefficient[2,1]) * 100
summary(m6)$coefficient[11,1] / (summary(m6)$coefficient[1,1] + summary(m6)$coefficient[2,1]) * 100

data1 = data.sex[data.sex$sex==0 & (data.sex$time <= 65 | data.sex$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m1)$coefficients[11,1], quantile(bsdat_1a[,11], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[11,1], quantile(bsdat_2a[,11], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[11,1], quantile(bsdat_3a[,11], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

data1 = data.sex[data.sex$sex==1 & (data.sex$time <= 65 | data.sex$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m4)$coefficients[11,1], quantile(bsdat_1a[,11], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[11,1], quantile(bsdat_2a[,11], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[11,1], quantile(bsdat_3a[,11], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)


# Step 6. Stratified Models by Age ####
data.age$age = as.factor(data.age$age)
data.age$decrim = as.factor(data.age$decrim)
data.age$expose = as.factor(data.age$expose)

# ITS for Philadelphia
m1 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & data.age$expose==1 & (data.age$time <= 65 | data.age$time >=70),])
m2 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & data.age$expose==1 & (data.age$time <= 65 | data.age$time >=70),])
m3 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & data.age$expose==1 & (data.age$time <= 65 | data.age$time >=70),])
m4 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & data.age$expose==1 & (data.age$time <= 65 | data.age$time >=70),])
m5 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & data.age$expose==1 & (data.age$time <= 65 | data.age$time >=70),])
m6 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & data.age$expose==1 & (data.age$time <= 65 | data.age$time >=70),])

data1 = data.age[data.age$age==0 & data.age$expose==1 & (data.age$time <= 65 | data.age$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m1)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

data1 = data.age[data.age$age==1 & data.age$expose==1 & (data.age$time <= 65 | data.age$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m4)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)

# ITS for Dauphin 
m1 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & data.age$expose==0 & (data.age$time <= 65 | data.age$time >=70),])
m2 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & data.age$expose==0 & (data.age$time <= 65 | data.age$time >=70),])
m3 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & data.age$expose==0 & (data.age$time <= 65 | data.age$time >=70),])
m4 = lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & data.age$expose==0 & (data.age$time <= 65 | data.age$time >=70),])
m5 = lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & data.age$expose==0 & (data.age$time <= 65 | data.age$time >=70),])
m6 = lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & data.age$expose==0 & (data.age$time <= 65 | data.age$time >=70),])

data1 = data.age[data.age$age==0 & data.age$expose==0 & (data.age$time <= 65 | data.age$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m1)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

data1 = data.age[data.age$age==1 & data.age$expose==0 & (data.age$time <= 65 | data.age$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
}

c(summary(m4)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)


# DID Regression
m1 = lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & (data.age$time <= 65 | data.age$time >=70),])
m2 = lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & (data.age$time <= 65 | data.age$time >=70),])
m3 = lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==0 & (data.age$time <= 65 | data.age$time >=70),])
m4 = lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & (data.age$time <= 65 | data.age$time >=70),])
m5 = lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & (data.age$time <= 65 | data.age$time >=70),])
m6 = lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, data = data.age[data.age$age==1 & (data.age$time <= 65 | data.age$time >=70),])


acf(residuals(m1,type="deviance"))
pacf(residuals(m1,type="deviance"))
acf(residuals(m2,type="deviance"))
pacf(residuals(m2,type="deviance"))
acf(residuals(m3,type="deviance"))
pacf(residuals(m3,type="deviance"))
acf(residuals(m4,type="deviance"))
pacf(residuals(m4,type="deviance"))
acf(residuals(m5,type="deviance"))
pacf(residuals(m5,type="deviance"))
acf(residuals(m6,type="deviance"))
pacf(residuals(m6,type="deviance"))

summary(m1); summary(m2); summary(m3);summary(m4); summary(m5); summary(m6);
round(ci.lin(m1,Exp=F),3); round(ci.lin(m2,Exp=F),3); round(ci.lin(m3,Exp=F),3); 
round(ci.lin(m4,Exp=F),3); round(ci.lin(m5,Exp=F),3); round(ci.lin(m6,Exp=F),3)

# Calculate percentage change 
summary(m1)$coefficient[11,1] / (summary(m1)$coefficient[1,1] + summary(m1)$coefficient[2,1]) * 100
summary(m2)$coefficient[11,1] / (summary(m2)$coefficient[1,1] + summary(m2)$coefficient[2,1]) * 100
summary(m3)$coefficient[11,1] / (summary(m3)$coefficient[1,1] + summary(m3)$coefficient[2,1]) * 100
summary(m4)$coefficient[11,1] / (summary(m4)$coefficient[1,1] + summary(m4)$coefficient[2,1]) * 100
summary(m5)$coefficient[11,1] / (summary(m5)$coefficient[1,1] + summary(m5)$coefficient[2,1]) * 100
summary(m6)$coefficient[11,1] / (summary(m6)$coefficient[1,1] + summary(m6)$coefficient[2,1]) * 100

data1 = data.age[data.age$age==0 & (data.age$time <= 65 | data.age$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m1)$coefficients[11,1], quantile(bsdat_1a[,11], c(0.002777778, 0.9972222)))
c(summary(m2)$coefficients[11,1], quantile(bsdat_2a[,11], c(0.002777778, 0.9972222)))
c(summary(m3)$coefficients[11,1], quantile(bsdat_3a[,11], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)

data1 = data.age[data.age$age==1 & (data.age$time <= 65 | data.age$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m2abs <- lm(rate.pos ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  m3abs <- lm(rate.man ~ expose*decrim + time + harmonic(month,2,12) + rate.police + medmj_lag, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  
}

c(summary(m4)$coefficients[11,1], quantile(bsdat_1a[,11], c(0.002777778, 0.9972222)))
c(summary(m5)$coefficients[11,1], quantile(bsdat_2a[,11], c(0.002777778, 0.9972222)))
c(summary(m6)$coefficients[11,1], quantile(bsdat_3a[,11], c(0.002777778, 0.9972222)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m4,m5,m6, data1)


# Sensitivity analysis: Time-lag
# time-lag by 1, 3, and 6 months for decriminalization 
data$decrim1 <- ifelse(data$time>=71,1,0)
data$decrim2 <- ifelse(data$time>=73,1,0)
data$decrim3 <- ifelse(data$time>=76,1,0)

m1 = lm(rate.tot ~ expose*decrim1 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m2 = lm(rate.pos ~ expose*decrim1 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m3 = lm(rate.man ~ expose*decrim1 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m4 = lm(rate.tot ~ expose*decrim2 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m5 = lm(rate.pos ~ expose*decrim2 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m6 = lm(rate.man ~ expose*decrim2 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m7 = lm(rate.tot ~ expose*decrim3 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m8 = lm(rate.pos ~ expose*decrim3 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])
m9 = lm(rate.man ~ expose*decrim3 + time + medmj_lag + harmonic(month,2,12) + rate.police, data = data[(data$time <= 65 | data$time >=70),])

# Bootstrapping
data1 = data[(data$time <= 65 | data$time >=70),]

set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL
bsdat_4a = NULL
bsdat_5a = NULL
bsdat_6a = NULL
bsdat_7a = NULL
bsdat_8a = NULL
bsdat_9a = NULL

for (i in 1:1000){
  dstar <- data1[sample(1:nrow(data1), size=nrow(data1), replace=T), ]
  m1abs <- lm(rate.tot ~ expose*decrim1 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m2abs <- lm(rate.pos ~ expose*decrim1 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m3abs <- lm(rate.man ~ expose*decrim1 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m4abs <- lm(rate.tot ~ expose*decrim2 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m5abs <- lm(rate.pos ~ expose*decrim2 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m6abs <- lm(rate.man ~ expose*decrim2 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m7abs <- lm(rate.tot ~ expose*decrim3 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m8abs <- lm(rate.pos ~ expose*decrim3 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  m9abs <- lm(rate.man ~ expose*decrim3 + time + medmj_lag + harmonic(month,2,12) + rate.police, dstar)
  
  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  bsdat_4a <- rbind(bsdat_4a, coef(m4abs))
  bsdat_5a <- rbind(bsdat_5a, coef(m5abs))
  bsdat_6a <- rbind(bsdat_6a, coef(m6abs))
  bsdat_7a <- rbind(bsdat_7a, coef(m7abs))
  bsdat_8a <- rbind(bsdat_8a, coef(m8abs))
  bsdat_9a <- rbind(bsdat_9a, coef(m9abs))
  
}

c(summary(m1)$coefficients[11,1], quantile(bsdat_1a[,11], c(0.025, 0.975)))
c(summary(m2)$coefficients[11,1], quantile(bsdat_2a[,11], c(0.025, 0.975)))
c(summary(m3)$coefficients[11,1], quantile(bsdat_3a[,11], c(0.025, 0.975)))
c(summary(m4)$coefficients[11,1], quantile(bsdat_4a[,11], c(0.025, 0.975)))
c(summary(m5)$coefficients[11,1], quantile(bsdat_5a[,11], c(0.025, 0.975)))
c(summary(m6)$coefficients[11,1], quantile(bsdat_6a[,11], c(0.025, 0.975)))
c(summary(m7)$coefficients[11,1], quantile(bsdat_7a[,11], c(0.025, 0.975)))
c(summary(m8)$coefficients[11,1], quantile(bsdat_8a[,11], c(0.025, 0.975)))
c(summary(m9)$coefficients[11,1], quantile(bsdat_9a[,11], c(0.025, 0.975)))

rm(i,bsdat_1a,bsdat_2a,bsdat_3a,m1abs,m2abs,m3abs,dstar,m1,m2,m3, data1)


