################################################################################
#   Interrupted time series regression for the evaluation of arrests related 
#     marijuana possession 
#   N. Tran; N. Goldstein; J. Purtle; P. Massey; S. Lankenau; J Suder; L. Tabb
################################################################################

#load packages
library(foreign) ; library(tsModel) ; library(Epi); library(dplyr); library(pscl); library(epiR); 
library(boot); library(RColorBrewer); library(ggplot2); library(ggthemes); library(gridExtra)

# read data from csv file
setwd("")
data <- read.csv("MarijuanaArrest.csv")

# compute the arrest rates
# data$philly.rate.tot <- with(data, philly.arrest.tot/pop*10^5)
# data$control.rate.tot <- with(data, control.arrest.tot/control.pop*10^5)
# data$philly.rate.pos <- with(data, philly.arrest.pos/pop*10^5)
# data$control.rate.pos <- with(data, control.arrest.pos/control.pop*10^5)
# data$philly.rate.man <- with(data, philly.arrest.man/pop*10^5)
# data$control.rate.man <- with(data, control.arrest.man/control.pop*10^5)

# include 22-month lag for medical marijuana legalization 
data$medmj_lag <- ifelse(data$time >= 110,1,0)

# Step 1: Descriptive analyses ####
# tabulate arrest rates before and after the decriminalization 

# subset to pre/post decriminalization
data1 <- data[data$decrim==0,] # pre-policy
data2 <- data[data$decrim==1,] # post-policy

# all arrests pre-decriminalization; possession then sales/manufacturing then total; Philadelphia then Controls
for (i in 1:12){
  print(sapply(data1[i], mean)); print(sapply(data1[i], sd))
}

# all arrests post-decriminalization; possession then sales/manufacturing then total; Philadelphia then Controls
for (i in 1:12){
  print(sapply(data2[i], mean)); print(sapply(data2[i], sd))
}

rm(i, data1, data2)

# Step 2: Qausi-Poisson Regression ####
# Crude Analysis for Philadelphia 
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Adjusted Analysis for Philadelphia 
m1 = glm(philly.arrest.tot ~ offset(log(pop)) + decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m2 = glm(philly.arrest.pos ~ offset(log(pop)) + decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m3 = glm(philly.arrest.man ~ offset(log(pop)) + decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)

#summary(m1); summary(m2); summary(m3)
round(ci.lin(m1,Exp=T),3); round(ci.lin(m2,Exp=T),3); round(ci.lin(m3,Exp=T),3)

# Check autocorrelation 
par(mfrow=c(1,2))
acf(residuals(m1,type="deviance"))
pacf(residuals(m1,type="deviance"))
acf(residuals(m2,type="deviance"))
pacf(residuals(m2,type="deviance"))
acf(residuals(m3,type="deviance"))
pacf(residuals(m3,type="deviance"))

# Crude Analysis for Controls 
lapply(c("control.arrest.tot","control.arrest.pos","control.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(control.pop)) + decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Adjusted Analysis for Controls 
m4 = glm(control.arrest.tot ~ offset(log(control.pop)) + decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m5 = glm(control.arrest.pos ~ offset(log(control.pop)) + decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m6 = glm(control.arrest.man ~ offset(log(control.pop)) + decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)

#summary(m4); summary(m5); summary(m6)
round(ci.lin(m4,Exp=T),3); round(ci.lin(m5,Exp=T),3); round(ci.lin(m6,Exp=T),3)

# Check autocorrelation
par(mfrow=c(1,2))
acf(residuals(m4,type="deviance"))
pacf(residuals(m4,type="deviance"))
acf(residuals(m5,type="deviance"))
pacf(residuals(m5,type="deviance"))
acf(residuals(m6,type="deviance"))
pacf(residuals(m6,type="deviance"))

# Plot temporal trends of arrest rates ####
# For total arrests ####
# prediction line for philadelphia then controls 
datanew <- data.frame(pop=mean(data$pop),decrim=rep(c(0,1),c(700,500)), medmj_lag=rep(c(0,1),c(1090,110)),time= 1:1200/10,month=rep(1:120/10,5),group=rep(c("Pre-policy","Post-policy"), c(700, 1200 - 700)), date=rep(seq(as.Date('2009/01/01'), as.Date('2018/12/01'), by="month")))
datanew1 <- data.frame(control.pop=mean(data$control.pop),decrim=rep(c(0,1),c(700,500)), medmj_lag=rep(c(0,1),c(1090,110)),time= 1:1200/10,month=rep(1:120/10,5),group=rep(c("Pre-policy","Post-policy"), c(700, 1200 - 700)), date=rep(seq(as.Date('2009/01/01'), as.Date('2018/12/01'), by="month")))
pred1 <- predict(m1,type="response",datanew)/mean(data$pop)*10^5
pred2 <- predict(m4,type="response",datanew1)/mean(data$control.pop)*10^5

# prediction line if decriminalization policy was not in place 
datanewb <- data.frame(pop=mean(data$pop),decrim=0, medmj_lag=rep(c(0,1),c(1090,110)),time= 1:1200/10,month=rep(1:120/10,5),group=rep(c("Pre-policy","Post-policy"), c(700, 1200 - 700)), date=rep(seq(as.Date('2009/01/01'), as.Date('2018/12/01'), by="month")))
datanew1b <- data.frame(control.pop=mean(data$control.pop),decrim=0, medmj_lag=rep(c(0,1),c(1090,110)),time= 1:1200/10,month=rep(1:120/10,5),group=rep(c("Pre-policy","Post-policy"), c(700, 1200 - 700)),date=rep(seq(as.Date('2009/01/01'), as.Date('2018/12/01'), by="month")))
pred1b <- predict(m1,type="response",datanewb)/mean(data$pop)*10^5
pred2b <- predict(m4,type="response",datanew1b)/mean(data$control.pop)*10^5 

# Plotting predicted values 
p <- ggplot(datanew, aes(x = time, y = pred1)) +
     theme_economist_white(gray_bg = FALSE, horizontal = TRUE) +
     labs(colour="") +
     ylab('Arrest Rate per 100 000') + 
     xlab('Year') +
     theme(axis.line.x = element_line(color="grey", size = 1),
        #axis.line.y = element_line(color="black", size = .1),
        axis.ticks.x = element_line(color = "grey", size = 1),
        axis.ticks.y = element_line(color = "grey", size = 1),
        axis.ticks.length=unit(0.25, "cm"),
        legend.position = "none"
        #legend.position = c(0.8, 0.8),
        #panel.grid.major = element_blank(),
        #legend.title=element_blank(),
        #legend.background = element_rect(linetype="solid",colour ="grey")
  )
p1 <- p + geom_line(aes(y=pred1b, col="Counterfactual"),size=1,alpha=1, linetype=5, show.legend = FALSE) + 
        geom_line(aes(y=pred1, col="Philadelphia"), size=1, alpha=1) + 
        geom_line(aes(y=pred2, col="Control"), size=1, alpha=1) + 
        annotate("rect", xmin = 70, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
        scale_color_manual(values=c("#FFC600", "#696969","#1739B6")) + 
        scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 55)) +
        geom_point(aes(x = time, y = philly.rate.tot,col="Philadelphia"),data=data,alpha=0.75,size=1) +  
        geom_point(aes(x = time, y = control.rate.tot,col="Control"),data=data,alpha=0.75,size=1) +
        ggtitle("A. All Marijuana Related Arrests") + theme(plot.title = element_text(lineheight=.8 ,face="bold", hjust=0))

# For arrests related to possession ####
# prediction line for philadelphia then controls 
pred3 <- predict(m2,type="response",datanew)/mean(data$pop)*10^5
pred4 <- predict(m5,type="response",datanew1)/mean(data$control.pop)*10^5

# prediction line if decriminalization policy was not in place 
pred3b <- predict(m2,type="response",datanewb)/mean(data$pop)*10^5
pred4b <- predict(m5,type="response",datanew1b)/mean(data$control.pop)*10^5

# Plotting predicted values 
p2 <- p + geom_line(aes(y=pred3b, col="Counterfactual"),size=1,alpha=1, linetype=5, show.legend = FALSE) + 
      geom_line(aes(y=pred3, col="Philadelphia"), size=1, alpha=1) + 
      geom_line(aes(y=pred4, col="Control"), size=1, alpha=1) + 
      annotate("rect", xmin = 70, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
      scale_color_manual(values=c("#FFC600", "#696969","#1739B6")) + 
      scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 55)) + 
      geom_point(aes(x = time, y = philly.rate.pos,col="Philadelphia"),data=data,alpha=0.75,size=1) +  
      geom_point(aes(x = time, y = control.rate.pos,col="Control"),data=data,alpha=0.75,size=1) +
      ggtitle("B. Arrests Related to Possession") + theme(plot.title = element_text(lineheight=.8 ,face="bold",hjust=0))

# For arrests related to sales and manufacturing ####
# prediction line for philadelphia then controls 
pred5 <- predict(m3,type="response",datanew)/mean(data$pop)*10^5
pred6 <- predict(m6,type="response",datanew1)/mean(data$control.pop)*10^5

# prediction line if decriminalization policy was not in place 
pred5b <- predict(m3,type="response",datanewb)/mean(data$pop)*10^5
pred6b <- predict(m6,type="response",datanew1b)/mean(data$control.pop)*10^5

p3 <- ggplot(datanew, aes(x = time, y = pred3)) +
  geom_line(aes(y=pred5b, col="Counterfactual"),size=1,alpha=1, linetype=5, show.legend = FALSE) + 
  # geom_smooth(method="lm", se=TRUE, aes(colour=group)) +
  theme_economist_white(gray_bg = FALSE, horizontal = TRUE) +
  labs(colour="") +
  ylab('Arrest Rate per 100 000') + 
  xlab('Year') +
  theme(axis.line.x = element_line(color="grey", size = 1),
        #axis.line.y = element_line(color="black", size = .1),
        axis.ticks.x = element_line(color = "grey", size = 1),
        axis.ticks.y = element_line(color = "grey", size = 1),
        axis.ticks.length=unit(0.25, "cm"),
        #legend.position = "none"
        legend.position = c(0.8, 0.8),
        #panel.grid.major = element_blank(),
        legend.title=element_blank(),
        legend.background = element_rect(linetype="solid",colour ="grey")
  )
p3 <- p3 + geom_line(aes(y=pred5, col="Philadelphia"), size=1, alpha=1) + 
      geom_line(aes(y=pred6, col="Control"), size=1, alpha=1) + 
      annotate("rect", xmin = 70, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
      scale_color_manual(values=c("#FFC600", "#696969","#1739B6")) + 
      scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 55)) +
      geom_point(aes(x = time, y = philly.rate.man,col="Philadelphia"),data=data,alpha=0.75,size=1) +  
      geom_point(aes(x = time, y = control.rate.man,col="Control"),data=data,alpha=0.75,size=1) +
      ggtitle("C. Arrests Related to Sales & Manufacturing") + theme(plot.title = element_text(lineheight=.8 ,face="bold", hjust=0))

# Arrange plot on one figure 
fig1 <- grid.arrange(p1, p2, p3, nrow = 1)
ggsave(filename = 'Figure 1.png', fig1, width = 17, height = 4.5, dpi = 600)

rm(datanew,datanew1,datanew1b,datanewb,m1,m2,m3,m4,m5,m6,pred1,pred1b,pred2,pred2b,pred3,pred3b,pred4,pred4b,pred5,pred5b,pred6,pred6b,fig1,p1,p2,p3)

# Step 3: Risk Difference ####
# Crude Analysis for Philadelphia then Controls
m1 <- glm(philly.rate.tot ~ decrim, family=quasipoisson(link = "identity"), data)
m2 <- glm(philly.rate.pos ~ decrim, family=quasipoisson(link = "identity"), data)
m3 <- glm(philly.rate.man ~ decrim, family=quasipoisson(link = "identity"), data)
m4 <- glm(control.rate.tot ~ decrim, family=quasipoisson(link = "identity"), data)
m5 <- glm(control.rate.pos ~ decrim, family=quasipoisson(link = "identity"), data)
m6 <- glm(control.rate.man ~ decrim, family=quasipoisson(link = "identity"), data)

# Adjusted Analysis for Phildelphia then Controls
m1a <- glm(philly.rate.tot ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), data)
m2a <- glm(philly.rate.pos ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), data)
m3a <- glm(philly.rate.man ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), data)
m4a <- glm(control.rate.tot ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), data)
m5a <- glm(control.rate.pos ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), data)
m6a <- glm(control.rate.man ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), data)

# Bootstrapping
# Crude Risk difference
set.seed(777)
bsdat1 = NULL
bsdat2 = NULL
bsdat3 = NULL
bsdat4 = NULL
bsdat5 = NULL
bsdat6 = NULL
for (i in 1:1000){
  dstar <- data[sample(1:nrow(data), size=nrow(data), replace=T), ]
  m1bs <- glm(philly.rate.tot ~ decrim, family=quasipoisson(link = "identity"), dstar)
  m2bs <- glm(philly.rate.pos ~ decrim, family=quasipoisson(link = "identity"), dstar)
  m3bs <- glm(philly.rate.man ~ decrim, family=quasipoisson(link = "identity"), dstar)
  m4bs <- glm(control.rate.tot ~ decrim, family=quasipoisson(link = "identity"), dstar)
  m5bs <- glm(control.rate.pos ~ decrim, family=quasipoisson(link = "identity"), dstar)
  m6bs <- glm(control.rate.man ~ decrim, family=quasipoisson(link = "identity"), dstar)
  
  bsdat1 <- rbind(bsdat1, coef(m1bs))
  bsdat2 <- rbind(bsdat2, coef(m2bs))
  bsdat3 <- rbind(bsdat3, coef(m3bs))
  bsdat4 <- rbind(bsdat4, coef(m4bs))
  bsdat5 <- rbind(bsdat5, coef(m5bs))
  bsdat6 <- rbind(bsdat6, coef(m6bs))
}

c(summary(m1)$coefficients[2,1], quantile(bsdat1[,2], c(0.025, 0.975)))
c(summary(m2)$coefficients[2,1], quantile(bsdat2[,2], c(0.025, 0.975)))
c(summary(m3)$coefficients[2,1], quantile(bsdat3[,2], c(0.025, 0.975)))
c(summary(m4)$coefficients[2,1], quantile(bsdat4[,2], c(0.025, 0.975)))
c(summary(m5)$coefficients[2,1], quantile(bsdat5[,2], c(0.025, 0.975)))
c(summary(m6)$coefficients[2,1], quantile(bsdat6[,2], c(0.025, 0.975)))

rm(m1,m2,m3,m4,m5,m6,bsdat1,bsdat2,bsdat3,bsdat4,bsdat5,bsdat6,m1bs,m2bs,m3bs,m4bs,m5bs,m6bs,dstar)

# Ajdusted risk difference 
set.seed(777)
bsdat_1a = NULL
bsdat_2a = NULL
bsdat_3a = NULL
bsdat_4a = NULL
bsdat_5a = NULL
bsdat_6a = NULL

for (i in 1:1000){
  dstar <- data[sample(1:nrow(data), size=nrow(data), replace=T), ]
  m1abs <- glm(philly.rate.tot ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), dstar)
  m2abs <- glm(philly.rate.pos ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), dstar)
  m3abs <- glm(philly.rate.man ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), dstar)
  m4abs <- glm(control.rate.tot ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), dstar)
  m5abs <- glm(control.rate.pos ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), dstar)
  m6abs <- glm(control.rate.man ~ decrim + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link = "identity"), dstar)

  bsdat_1a <- rbind(bsdat_1a, coef(m1abs))
  bsdat_2a <- rbind(bsdat_2a, coef(m2abs))
  bsdat_3a <- rbind(bsdat_3a, coef(m3abs))
  bsdat_4a <- rbind(bsdat_4a, coef(m4abs))
  bsdat_5a <- rbind(bsdat_5a, coef(m5abs))
  bsdat_6a <- rbind(bsdat_6a, coef(m6abs))
}

c(summary(m1a)$coefficients[2,1], quantile(bsdat_1a[,2], c(0.025, 0.975)))
c(summary(m2a)$coefficients[2,1], quantile(bsdat_2a[,2], c(0.025, 0.975)))
c(summary(m3a)$coefficients[2,1], quantile(bsdat_3a[,2], c(0.025, 0.975)))
c(summary(m4a)$coefficients[2,1], quantile(bsdat_4a[,2], c(0.025, 0.975)))
c(summary(m5a)$coefficients[2,1], quantile(bsdat_5a[,2], c(0.025, 0.975)))
c(summary(m6a)$coefficients[2,1], quantile(bsdat_6a[,2], c(0.025, 0.975)))

rm(i,m1a,m2a,m3a,m4a,m5a,m6a,bsdat_1a,bsdat_2a,bsdat_3a,bsdat_4a,bsdat_5a,bsdat_6a,m1abs,m2abs,m3abs,m4abs,m5abs,m6abs,dstar)

# Sensitivity Analysis ####
# time-lag by 1, 3, 6, and 12 months for decriminalization 
data$decrim1 <- ifelse(data$time>=72,1,0)
data$decrim2 <- ifelse(data$time>=74,1,0)
data$decrim3 <- ifelse(data$time>=77,1,0)
data$decrim4 <- ifelse(data$time>=83,1,0)

lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + decrim1 + time + medmj_lag + harmonic(month,2,12)"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + decrim2 + time + medmj_lag + harmonic(month,2,12)"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + decrim3 + time + medmj_lag + harmonic(month,2,12)"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + decrim4 + time + medmj_lag + harmonic(month,2,12)"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Plots Sensitivity Results ####
dat <- matrix(c(0.501,0.519,0.575,
                0.724,0.272,0.285,
                0.313,0.422,0.800,
                0.804,0.867,0.973,
                0.450,0.463,0.504,
                0.617,0.240,0.246,
                0.262,0.330,0.707,
                0.712,0.766,0.859,
                0.558,0.582,0.656,
                0.848,0.308,0.330,
                0.374,0.540,0.905,
                0.909,0.981,1.103),
              nrow=12,ncol=3)
dat <- as.data.frame(dat)
dat$time <- 1:12

par(mfrow=c(1,3))
# Graph 1: All Decrim
plot(x=dat[1:4,]$time, y=dat[1:4,]$V1,type="n",ylim=c(0.2,1.6),ylab="Relative Risk (95% CI)", xlab="Time Lags", bty="l",xaxt="n", log = "y")
points(x=dat[1:4,]$time, y=dat[1:4,]$V1, pch=19, cex=1.5)
arrows(x0=dat[1:4,]$time, y0=dat[1:4,]$V2, x1=dat[1:4,]$time, y1=dat[1:4,]$V3,angle=90, length=0.05, code=3, lwd=2)
axis(side=1, at=dat[1:4,]$time, labels=c("1-month","3-months","6-months","12-months"))
abline(h=1,lty=2,lwd=2,col="red")
title(main="A. All Arrests", adj=0)

# Graph 2: Decrim Poss
plot(x=dat[5:8,]$time, y=dat[5:8,]$V1,type="n",ylim=c(0.2,1.6),ylab="Relative Risk (95% CI)", xlab="Time Lags", bty="l",xaxt="n", log = "y")
points(x=dat[5:8,]$time, y=dat[5:8,]$V1, pch=19, cex=1.5)
arrows(x0=dat[5:8,]$time, y0=dat[5:8,]$V2, x1=dat[5:8,]$time, y1=dat[5:8,]$V3,angle=90, length=0.05, code=3, lwd=2)
axis(side=1, at=dat[5:8,]$time, labels=c("1-month","3-months","6-months","12-months"))
abline(h=1,lty=2,lwd=2,col="red")
title(main="B. Arrests for Possession" , adj=0)

# Graph 3: Decrim Manu
plot(x=dat[9:12,]$time, y=dat[9:12,]$V1,type="n",ylim=c(0.2,1.6),ylab="Relative Risk (95% CI)", xlab="Time Lags", bty="l",xaxt="n", log = "y")
points(x=dat[9:12,]$time, y=dat[9:12,]$V1, pch=19, cex=1.5)
arrows(x0=dat[9:12,]$time, y0=dat[9:12,]$V2, x1=dat[9:12,]$time, y1=dat[9:12,]$V3,angle=90, length=0.05, code=3, lwd=2)
axis(side=1, at=dat[9:12,]$time, labels=c("1-month","3-months","6-months","12-months"))
abline(h=1,lty=2,lwd=2,col="red")
title( main="C. Arrests for Sales & Manufacturing", adj=0)

rm(dat)

# Multiple Baseline Analysis ####
data$decrim_pitt <- ifelse(data$time >=84,1,0)
data$decrim_har <- ifelse(data$time >=91,1,0)

m1 = glm(pitt.arrest.tot ~ offset(log(pitt.pop)) + decrim_pitt + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m2 = glm(pitt.arrest.pos ~ offset(log(pitt.pop)) + decrim_pitt + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m3 = glm(pitt.arrest.man ~ offset(log(pitt.pop)) + decrim_pitt + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m4 = glm(har.arrest.tot ~ offset(log(har.pop)) + decrim_har + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m5 = glm(har.arrest.pos ~ offset(log(har.pop)) + decrim_har + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)
m6 = glm(har.arrest.man ~ offset(log(har.pop)) + decrim_har + time + medmj_lag + harmonic(month,2,12), family=quasipoisson(link="log"), data)

round(ci.lin(m1,Exp=T),3); round(ci.lin(m2,Exp=T),3); round(ci.lin(m3,Exp=T),3)
round(ci.lin(m4,Exp=T),3); round(ci.lin(m5,Exp=T),3); round(ci.lin(m6,Exp=T),3)

# Plot temporal trends of arrest rates for Pittsburgh then Harrisburg 
# For total arrests 
# prediction line for Pittsburgh then Harrisburg
datanew <- data.frame(pitt.pop=mean(data$pitt.pop),decrim_pitt=rep(c(0,1),c(830,370)), medmj_lag=rep(c(0,1),c(1090,110)),time= 1:1200/10,month=rep(1:120/10,5),group=rep(c("Pre-policy","Post-policy"), c(700, 1200 - 700)), date=rep(seq(as.Date('2009/01/01'), as.Date('2018/12/01'), by="month")))
datanew1 <- data.frame(har.pop=mean(data$har.pop),decrim_har=rep(c(0,1),c(900,300)), medmj_lag=rep(c(0,1),c(1090,110)),time= 1:1200/10,month=rep(1:120/10,5),group=rep(c("Pre-policy","Post-policy"), c(700, 1200 - 700)), date=rep(seq(as.Date('2009/01/01'), as.Date('2018/12/01'), by="month")))
pred1 <- predict(m1,type="response",datanew)/mean(data$pitt.pop)*10^5
pred2 <- predict(m2,type="response",datanew)/mean(data$pitt.pop)*10^5
pred3 <- predict(m3,type="response",datanew)/mean(data$pitt.pop)*10^5
pred4 <- predict(m4,type="response",datanew1)/mean(data$har.pop)*10^5
pred5 <- predict(m5,type="response",datanew1)/mean(data$har.pop)*10^5
pred6 <- predict(m6,type="response",datanew1)/mean(data$har.pop)*10^5

# Plotting trend for Pittsburgh
p_pitt <- ggplot(datanew, aes(x = time, y = pred1)) +
  # geom_smooth(method="lm", se=TRUE, aes(colour=group)) +
  theme_economist_white(gray_bg = FALSE, horizontal = TRUE) +
  labs(colour="") +
  ylab('Arrest Rate per 100 000') + 
  xlab('Year') +
  theme(axis.line.x = element_line(color="grey", size = 1),
        #axis.line.y = element_line(color="black", size = .1),
        axis.ticks.x = element_line(color = "grey", size = 1),
        axis.ticks.y = element_line(color = "grey", size = 1),
        axis.ticks.length=unit(0.25, "cm"),
        legend.position = "none"
        #legend.position = c(0.8, 0.8),
        #panel.grid.major = element_blank(),
        #legend.title=element_blank(),
        #legend.background = element_rect(linetype="solid",colour ="grey")
        )

p_pitt1 <- p_pitt + geom_line(aes(y=pred1, col="Pittsburgh"), size=1, alpha=1) + 
  annotate("rect", xmin = 83, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
  scale_color_manual(values=c("#1739B6")) + 
  scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 90)) + 
  geom_point(aes(x = time, y = pitt.rate.tot,col="Pittsburgh"),data=data,alpha=0.75,size=1) +  
  ggtitle("A. All Marijuana Related Arrests") + theme(plot.title = element_text(lineheight=.8 ,face="bold",hjust=0))

p_pitt2 <- p_pitt + geom_line(aes(y=pred2, col="Pittsburgh"), size=1, alpha=1) + 
  annotate("rect", xmin = 83, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
  scale_color_manual(values=c("#1739B6")) + 
  scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 90)) + 
  geom_point(aes(x = time, y = pitt.rate.pos,col="Pittsburgh"),data=data,alpha=0.75,size=1) +  
  ggtitle("A. Arrests Related to Possession") + theme(plot.title = element_text(lineheight=.8 ,face="bold",hjust=0))

p_pitt3 <- p_pitt + geom_line(aes(y=pred3, col="Pittsburgh"), size=1, alpha=1) + 
  annotate("rect", xmin = 83, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
  scale_color_manual(values=c("#1739B6")) + 
  scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 90)) + 
  geom_point(aes(x = time, y = pitt.rate.man,col="Pittsburgh"),data=data,alpha=0.75,size=1) +  
  ggtitle("A. Arrests Related to Sales & Manufacturing") + theme(plot.title = element_text(lineheight=.8 ,face="bold",hjust=0))

figs1 <- grid.arrange(p_pitt1, p_pitt2, p_pitt3, nrow = 1)
ggsave(filename = 'Figure S1.png', figs1, width = 17, height = 4.5, dpi = 600)

rm(p_pitt,p_pitt1,p_pitt2,p_pitt3,figs1,pred1,pred2,pred3)

# Plotting trend for Harrisburg
p_har <- ggplot(datanew, aes(x = time, y = pred4)) +
  theme_economist_white(gray_bg = FALSE, horizontal = TRUE) +
  labs(colour="") +
  ylab('Arrest Rate per 100 000') + 
  xlab('Year') +
  theme(axis.line.x = element_line(color="grey", size = 1),
        #axis.line.y = element_line(color="black", size = .1),
        axis.ticks.x = element_line(color = "grey", size = 1),
        axis.ticks.y = element_line(color = "grey", size = 1),
        axis.ticks.length=unit(0.25, "cm"),
        legend.position = "none"
        #legend.position = c(0.8, 0.8),
        #panel.grid.major = element_blank(),
        #legend.title=element_blank(),
        #legend.background = element_rect(linetype="solid",colour ="grey")
  )

p_har1 <- p_har + geom_line(aes(y=pred4, col="Harrisburg"), size=1, alpha=1) + 
  annotate("rect", xmin = 90, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
  scale_color_manual(values=c("#1739B6")) + 
  scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 90)) +
  geom_point(aes(x = time, y = har.rate.tot,col="Harrisburg"),data=data,alpha=0.75,size=1) +  
  ggtitle("A. All Marijuana Related Arrests") + theme(plot.title = element_text(lineheight=.8 ,face="bold",hjust=0))

p_har2 <- p_har + geom_line(aes(y=pred5, col="Harrisburg"), size=1, alpha=1) + 
  annotate("rect", xmin = 90, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
  scale_color_manual(values=c("#1739B6")) + 
  scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 90)) +
  geom_point(aes(x = time, y = har.rate.pos,col="Harrisburg"),data=data,alpha=0.75,size=1) +  
  ggtitle("B. Arrests Related to Possession") + theme(plot.title = element_text(lineheight=.8 ,face="bold",hjust=0))

p_har3 <- p_har + geom_line(aes(y=pred6, col="Harrisburg"), size=1, alpha=1) + 
  annotate("rect", xmin = 90, xmax = Inf, ymin = 0, ymax = Inf, alpha = .1) + 
  scale_color_manual(values=c("#1739B6")) + 
  scale_x_continuous(expand = c(0, 0), breaks=seq(0,120,12),limits = c(0,120), labels = c(2009:2018,"")) + scale_y_continuous(expand = c(0, 0),limits = c(0, 90)) +
  geom_point(aes(x = time, y = har.rate.man,col="Harrisburg"),data=data,alpha=0.75,size=1) +  
  ggtitle("C. Arrests Related to Sales & Manufacturing") + theme(plot.title = element_text(lineheight=.8 ,face="bold",hjust=0))

figs2 <- grid.arrange(p_har1, p_har2, p_har3, nrow = 1)
ggsave(filename = 'Figure S2.png', figs2, width = 17, height = 4.5, dpi = 600)

rm(figs2,p_har,p_har1,p_har2,p_har3)

# Plot results of multiple baseline analsis
dat <- matrix(c(0.814,0.800,0.892,
                0.540,0.468,0.880,
                0.680,0.664,0.663,
                0.432,0.366,0.617,
                0.975,0.964,1.200,
                0.675,0.599,1.256),
              nrow=6,ncol=3)
dat <- as.data.frame(dat)
dat$time <- 1:6

par(mfrow=c(1,2))
# Graph 1: Plottig Pittsburgh results
plot(x=dat[1:3,]$time, y=dat[1:3,]$V1,type="n",ylim=c(0.2,1.3),ylab="Relative Risk (95% CI)", xlab="Types of Arrest", bty="l",xaxt="n", log = "y")
points(x=dat[1:3,]$time, y=dat[1:3,]$V1, pch=19, cex=1.5)
arrows(x0=dat[1:3,]$time, y0=dat[1:3,]$V2, x1=dat[1:3,]$time, y1=dat[1:3,]$V3,angle=90, length=0.05, code=3, lwd=2)
axis(side=1, at=dat[1:3,]$time, labels=c("All Arrests","Possession","Sales/Mfg"))
abline(h=1,lty=2,lwd=2,col="red")
title(main="A. Pittsburgh", adj=0)

# Graph 2: Plotting harrisburg results
plot(x=dat[4:6,]$time, y=dat[4:6,]$V1,type="n",ylim=c(0.2,1.3),ylab="Relative Risk (95% CI)", xlab="Types of Arrest", bty="l",xaxt="n", log = "y")
points(x=dat[4:6,]$time, y=dat[4:6,]$V1, pch=19, cex=1.5)
arrows(x0=dat[4:6,]$time, y0=dat[4:6,]$V2, x1=dat[4:6,]$time, y1=dat[4:6,]$V3,angle=90, length=0.05, code=3, lwd=2)
axis(side=1, at=dat[4:6,]$time, labels=c("All Arrests","Possession","Sales/Mfg"))
abline(h=1,lty=2,lwd=2,col="red")
title(main="B. Harrisburg" , adj=0)

rm(data,datanew,datanew1,m1,m2,m3,m4,m5,m6,pred1,pred2,pred3,pred4,pred5,pred6)


# Interaction Analysis ####

# By Sex; 1=male; 0=female ####
data = read.csv("ArrestBySex.csv",header=T,sep=",")

# data$philly.rate.tot <- with(data, philly.arrest.tot/pop*10^5)
# data$philly.rate.pos <- with(data, philly.arrest.pos/pop*10^5)
# data$philly.rate.man <- with(data, philly.arrest.man/pop*10^5)

# include 22-month lag for medical marijuana legalization 
data$medmj_lag <- ifelse(data$time >= 110,1,0)

# Step 1: Descriptive analyses ####
# subset to pre/post decriminalization
data1 <- data[data$decrim==0,] # pre-policy
  data1m <- data1[data1$sex==1,] 
  data1f <- data1[data1$sex==0,] 
data2 <- data[data$decrim==1,] # post-policy
  data2m <- data2[data2$sex==1,] 
  data2f <- data2[data2$sex==0,]

# all arrests pre-decriminalization; total then possession then sales/manufacturing; males then females 
for (i in 1:6){
  print(sapply(data1m[i], mean)); print(sapply(data1m[i], sd))
}

# all arrests pre-decriminalization; total then possession then sales/manufacturing; males then females 
for (i in 1:6){
  print(sapply(data1f[i], mean)); print(sapply(data1f[i], sd))
}

# all arrests post-decriminalization; total then possession then sales/manufacturing; males then females 
for (i in 1:6){
  print(sapply(data2m[i], mean)); print(sapply(data2m[i], sd))
}
  
# all arrests post-decriminalization; total then possession then sales/manufacturing; males then females 
for (i in 1:6){
  print(sapply(data2f[i], mean)); print(sapply(data2f[i], sd))
}  

rm(i, data1, data2, data1m, data1f, data2m, data2f)

# Step 2: Qausi-Poisson Regression ####
data$sex_cat = relevel(as.factor(data$sex), ref = "0")

# Crude Analysis for Philadelphia
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + sex_cat*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Adjusted Analysis for Philadelphia 
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + time + medmj + harmonic(month,2,12) + sex_cat*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Check autocorrelation 
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + time + medmj + harmonic(month,2,12) + sex_cat*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         par(mfrow=c(2,1))
         acf(residuals(res.poi,type="deviance"))
         pacf(residuals(res.poi,type="deviance"))
       })

# Step 3: Additive Interaction 
# re-parameterize as risk factors
data$sex_cat = relevel(as.factor(data$sex), ref = "0") # female is reference 
data$decrim_cat = relevel(as.factor(data$decrim), ref = "1") # post-decriminalization is reference 

# independent additive effect of age and decriminalization 
m1 <- glm(philly.arrest.tot ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + sex_cat*decrim_cat, family=quasipoisson(link = "log"), data)
m2 <- glm(philly.arrest.pos ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + sex_cat*decrim_cat, family=quasipoisson(link = "log"), data)
m3 <- glm(philly.arrest.man ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + sex_cat*decrim_cat, family=quasipoisson(link = "log"), data)

S1 = (exp(coef(m1)[9] + coef(m1)[8] + coef(m1)[10]) - 1) / ((exp(coef(m1)[9]) - 1) + (exp(coef(m1)[8]) - 1))
S2 = (exp(coef(m2)[9] + coef(m2)[8] + coef(m2)[10]) - 1) / ((exp(coef(m2)[9]) - 1) + (exp(coef(m2)[8]) - 1))
S3 = (exp(coef(m3)[9] + coef(m3)[8] + coef(m3)[10]) - 1) / ((exp(coef(m3)[9]) - 1) + (exp(coef(m3)[8]) - 1))

# Bootstrap
bootAdditive = function(data,index,method)
{
  bootdata = data[index,]
  model = glm(philly.arrest.man ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + sex_cat*decrim_cat, family=quasipoisson(link = "log"), data=bootdata)
  if (method=="RERI") {
    additive = exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - exp(coef(model)[9]) - exp(coef(model)[8]) + 1
  } else if (method=="AP") {
    RERI = exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - exp(coef(model)[9]) - exp(coef(model)[8]) + 1
    additive = RERI / exp(coef(model)[9] + coef(model)[8] + coef(model)[10])
  } else if (method=="Synergy") {
    additive = (exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - 1) / ((exp(coef(model)[9]) - 1) + (exp(coef(model)[8]) - 1))
  }
  return(as.numeric(additive))
}

boot_ci = boot(data, bootAdditive, 1000, method="Synergy", parallel="multicore", ncpus=4)
boot.ci(boot_ci, type="norm", index=1)

rm(boot_ci, data, m1,m2,m3, S1,S2,S3, bootAdditive)

# By Race; 1=Black; 0=White
data = read.csv("ArrestByRace.csv",header=T,sep=",")

# data$philly.rate.tot <- with(data, philly.arrest.tot/pop*10^5)
# data$philly.rate.pos <- with(data, philly.arrest.pos/pop*10^5)
# data$philly.rate.man <- with(data, philly.arrest.man/pop*10^5)

# Step 1: Descriptive analyses ####
# subset to pre/post decriminalization
data1 <- data[data$decrim==0,] # pre-policy
  data1w <- data1[data1$race==0,] 
  data1b <- data1[data1$race==1,] 
data2 <- data[data$decrim==1,] # post-policy
  data2w <- data2[data2$race==0,] 
  data2b <- data2[data2$race==1,]

# include 22-month lag for medical marijuana legalization 
data$medmj_lag <- ifelse(data$time >= 110,1,0)
  
# all arrests pre-decriminalization; total then possession then sales/manufacturing; black  
for (i in 1:6){
  print(sapply(data1b[i], mean)); print(sapply(data1b[i], sd))
}

# all arrests pre-decriminalization; total then possession then sales/manufacturing; white 
for (i in 1:6){
  print(sapply(data1w[i], mean)); print(sapply(data1w[i], sd))
}

# all arrests post-decriminalization; total then possession then sales/manufacturing; black then white 
for (i in 1:6){
  print(sapply(data2b[i], mean)); print(sapply(data2b[i], sd))
}

# all arrests post-decriminalization; total then possession then sales/manufacturing; black then white 
for (i in 1:6){
  print(sapply(data2w[i], mean)); print(sapply(data2w[i], sd))
}  

rm(i, data1, data2, data1b, data1w, data2b, data2w)

# Step 2: Qausi-Poisson Regression ####
# Crude Analysis for Philadelphia
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + race*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Adjusted Analysis for Philadelphia 
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + race*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Check autocorrelation 
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + race*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         par(mfrow=c(2,1))
         acf(residuals(res.poi,type="deviance"))
         pacf(residuals(res.poi,type="deviance"))
       })

# Step 3: Additive Interaction 
# re-parameterize to risk factors 
data$race_cat = relevel(as.factor(data$race), ref = "0")
data$decrim_cat = relevel(as.factor(data$decrim), ref = "1")

# independent additive effect of age and decriminalization 
m1 <- glm(philly.arrest.tot ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + race_cat*decrim_cat, family=quasipoisson(link = "log"), data)
m2 <- glm(philly.arrest.pos ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + race_cat*decrim_cat, family=quasipoisson(link = "log"), data)
m3 <- glm(philly.arrest.man ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + race_cat*decrim_cat, family=quasipoisson(link = "log"), data)

S1 = (exp(coef(m1)[9] + coef(m1)[8] + coef(m1)[10]) - 1) / ((exp(coef(m1)[9]) - 1) + (exp(coef(m1)[8]) - 1))
S2 = (exp(coef(m2)[9] + coef(m2)[8] + coef(m2)[10]) - 1) / ((exp(coef(m2)[9]) - 1) + (exp(coef(m2)[8]) - 1))
S3 = (exp(coef(m3)[9] + coef(m3)[8] + coef(m3)[10]) - 1) / ((exp(coef(m3)[9]) - 1) + (exp(coef(m3)[8]) - 1))

# Bootstrap
bootAdditive = function(data,index,method)
{
  bootdata = data[index,]
  model = glm(philly.arrest.man ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + race_cat*decrim_cat, family=quasipoisson(link = "log"), data=bootdata)
  if (method=="RERI") {
    additive = exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - exp(coef(model)[9]) - exp(coef(model)[8]) + 1
  } else if (method=="AP") {
    RERI = exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - exp(coef(model)[9]) - exp(coef(model)[8]) + 1
    additive = RERI / exp(coef(model)[9] + coef(model)[8] + coef(model)[10])
  } else if (method=="Synergy") {
    additive = (exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - 1) / ((exp(coef(model)[9]) - 1) + (exp(coef(model)[8]) - 1))
  }
  return(as.numeric(additive))
}

boot_ci = boot(data, bootAdditive, 1000, method="Synergy", parallel="multicore", ncpus=4)
boot.ci(boot_ci, type="norm", index=1)

rm(boot_ci, data, m1,m2,m3, S1,S2,S3, bootAdditive)


# By Age; 1=Adult; 0=Youth ####
data = read.csv("ArrestByAge.csv",header=T,sep=",")

# data$philly.rate.tot <- with(data, philly.arrest.tot/pop*10^5)
# data$philly.rate.pos <- with(data, philly.arrest.pos/pop*10^5)
# data$philly.rate.man <- with(data, philly.arrest.man/pop*10^5)

# include 22-month lag for medical marijuana legalization 
data$medmj_lag <- ifelse(data$time >= 110,1,0)

# Step 1: Descriptive analyses ####
# subset to pre/post decriminalization
data1 <- data[data$decrim==0,] # pre-policy
  data1a <- data1[data1$age==1,] 
  data1y <- data1[data1$age==0,] 
data2 <- data[data$decrim==1,] # post-policy
  data2a <- data2[data2$age==1,] 
  data2y <- data2[data2$age==0,]

# all arrests pre-decriminalization; possession then sales/manufacturing then total; adult then youth 
for (i in 1:6){
  print(sapply(data1a[i], mean)); print(sapply(data1a[i], sd))
}

# all arrests pre-decriminalization; total then possession then sales/manufacturing; adult then youth 
for (i in 1:6){
  print(sapply(data1y[i], mean)); print(sapply(data1y[i], sd))
}

# all arrests post-decriminalization; possession then sales/manufacturing then total; adult then youth 
for (i in 1:6){
  print(sapply(data2a[i], mean)); print(sapply(data2a[i], sd))
}

# all arrests post-decriminalization; possession then sales/manufacturing then total; adult then youth 
for (i in 1:6){
  print(sapply(data2y[i], mean)); print(sapply(data2y[i], sd))
}  

rm(i, data1, data2, data1a, data1y, data2a, data2y)

# Step 2: Qausi-Poisson Regression ####
# Crude Analysis for Philadelphia 
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + age*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Adjusted Analysis for Philadelphia 
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + age*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         #summary(res.poi)
         round(ci.lin(res.poi,Exp=T),3)
       })

# Check autocorrelation 
lapply(c("philly.arrest.tot","philly.arrest.pos","philly.arrest.man"), 
       function(var){
         formula <-  as.formula(paste(var, "~ offset(log(pop)) + time + medmj + harmonic(month,2,12) + age*decrim"))
         res.poi <- glm(formula, family=quasipoisson(link="log"), data)
         
         par(mfrow=c(2,1))
         acf(residuals(res.poi,type="deviance"))
         pacf(residuals(res.poi,type="deviance"))
       })

# Step 3: Additive Interaction 
# re-parameterize to risk factors 
data$age_cat = relevel(as.factor(data$age), ref = "0")
data$decrim_cat = relevel(as.factor(data$decrim), ref = "1")

# independent additive effect of age and decriminalization 
m1 <- glm(philly.arrest.tot ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + age_cat*decrim_cat, family=quasipoisson(link = "log"), data)
m2 <- glm(philly.arrest.pos ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + age_cat*decrim_cat, family=quasipoisson(link = "log"), data)
m3 <- glm(philly.arrest.man ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + age_cat*decrim_cat, family=quasipoisson(link = "log"), data)

S1 = (exp(coef(m1)[9] + coef(m1)[8] + coef(m1)[10]) - 1) / ((exp(coef(m1)[9]) - 1) + (exp(coef(m1)[8]) - 1))
S2 = (exp(coef(m2)[9] + coef(m2)[8] + coef(m2)[10]) - 1) / ((exp(coef(m2)[9]) - 1) + (exp(coef(m2)[8]) - 1))
S3 = (exp(coef(m3)[9] + coef(m3)[8] + coef(m3)[10]) - 1) / ((exp(coef(m3)[9]) - 1) + (exp(coef(m3)[8]) - 1))

# Bootstrap
bootAdditive = function(data,index,method)
{
  bootdata = data[index,]
  model = glm(philly.arrest.man ~ offset(log(pop)) + time + medmj_lag + harmonic(month,2,12) + age_cat*decrim_cat, family=quasipoisson(link = "log"), data=bootdata)
  if (method=="RERI") {
    additive = exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - exp(coef(model)[9]) - exp(coef(model)[8]) + 1
  } else if (method=="AP") {
    RERI = exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - exp(coef(model)[9]) - exp(coef(model)[8]) + 1
    additive = RERI / exp(coef(model)[9] + coef(model)[8] + coef(model)[10])
  } else if (method=="Synergy") {
    additive = (exp(coef(model)[9] + coef(model)[8] + coef(model)[10]) - 1) / ((exp(coef(model)[9]) - 1) + (exp(coef(model)[8]) - 1))
  }
  return(as.numeric(additive))
}

boot_ci = boot(data, bootAdditive, 1000, method="Synergy", parallel="multicore", ncpus=4)
boot.ci(boot_ci, type="norm", index=1)

rm(boot_ci, data, m1,m2,m3, S1,S2,S3, bootAdditive)