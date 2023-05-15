## QUESTION 1  ##
# From (i) to (v) #
rm(list=ls())
set.seed(123); options(scipen = 100)
library("dplyr");library("kableExtra");library("np");library('data.table');library("MASS")
library("rpart"); library("rpart.plot");library("parallel");library("matrixcalc");
library("purrr"); library("beepr"); library("ggplot2"); library("tseries")

# GENERATING DATA #
delta = as.matrix(c(1,1,1)); lambda=diag(c(0.1,0.5,0.9));
rep = 200;

x = matrix(0,rep,3); x[1,]=delta; 
for(i in 2:rep){
  lag = i-1
  x[i,]= delta + lambda%*%x[lag,] + rnorm(3,0,1)
}

x<-as.data.frame(x);x<-cbind(x,"T"=seq(1,rep))

colnames(x) = c("X1", "X2", "X3", "Time")



## Plots ##

ggplot(x, aes(x=Time, y=X1)) +
  geom_line() +  geom_hline(yintercept = 1.111, linetype="dotted",
                            color = "red", size=1) + 
  labs(title = "Evolution in X1", x = "Time",
       y = "Variation in X1")+
  theme_classic() 

ggplot(x, aes(x=Time, y=X2)) +
  geom_line() +  geom_hline(yintercept = 2, linetype="dotted",
                            color = "red", size=1) + 
  labs(title = "Evolution in X2", x = "Time",
       y = "Variation in X2")+
  theme_classic() 

ggplot(x, aes(x=Time, y=X3)) +
  geom_line() +  geom_hline(yintercept = 10, linetype="dotted",
                            color = "red", size=1) + 
  labs(title = "Evolution in X3", x = "Time",
       y = "Variation in X3")+
  theme_classic() 

## Sample mean ##

Mean<-colMeans(x[,1:3])

Diffmean<-colMeans(x[,1:3])-c(1/(1-0.1),1/(1-0.5),1/(1-0.9))

## Variance Sample ##

Var <- diag(cov(x[,1:3]))

DiffVar<-(diag(cov(x[,1:3]))-c(1/(1-0.1^2),1/(1-0.5^2),1/(1-0.9^2)))

## iv ##

## GENERATING DATA UNIT ROOT PROCESS##
delta = as.matrix(c(0,0,0)); lambda=diag(c(1,1,1));
rep = 200;

x = matrix(0,rep,3); x[1,]=delta; 
for(i in 2:rep){
  lag = i-1
  x[i,]= delta + lambda%*%x[lag,] + rnorm(3,0,1)
}

x<-as.data.frame(x);x<-cbind(x,"T"=seq(1,rep))

## Plots ## 

ggplot(x, aes(x=T, y=V1)) +
  geom_line()  + 
  labs(title = "Evolution in X1", x = "Time",
       y = "Variation in X1")+
  theme_classic() 

ggplot(x, aes(x=T, y=V2)) +
  geom_line() + 
  labs(title = "Evolution in X2", x = "Time",
       y = "Variation in X2")+
  theme_classic() 

ggplot(x, aes(x=T, y=V3)) +
  geom_line()  + 
  labs(title = "Evolution in X3", x = "Time",
       y = "Variation in X3")+
  theme_classic() 

## ix ##
## GENERATING DATA UNIVARIATE UNIT ROOT ##

delta = 0; lambda=1;
rep = 200;

x = matrix(0,rep,1); x[1,]=delta; 
for(i in 2:rep){
  lag = i-1
  x[i,]= delta + lambda*x[lag,] + rnorm(1,0,1)
}

X<-as.data.frame(x);X<-cbind(X,"T"=seq(1,rep))

## Plot ## 

ggplot(X, aes(x=T, y=V1)) +
  geom_line()  + 
  labs(title = "Evolution in x", x = "Time",
       y = "Variation in x")+
  theme_classic() 

##xi: test for unit root using DF test##

testtrend <- lm(V1~T, data = X)

# The coefficient of the trend is close to 0, then we can say that there is not trend linked to this univariate simulation #
# Note: The Dickey-Fuller test is a special case of Augmented Dickey-Fuller test when nlag = 2#
# D-F test: statistical test in which H0 is nons-tationary and HA holds for stationary#

adf.test(x, k = 2)

# We do not have enough evidence to reject the H0,i.e., by looking at the test we cannot say that the univariate unit root process is stationary. And by definition we already know it is non-stationary#

## QUESTION 2 ##

rm(list=ls())

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dplyr)

#Importing data
df <- read.csv("VU_MultivariateEcnmtrcs_assignment_dataset.csv")
df<- df[df$cntry.name == "India",]


#Data for plot in appendix
df2 <- data[, c("mean_pre", "mean_tmp", "AG.LND.AGRI.K2", "NV.AGR.TOTL.KD")]
df3 <- log(df2)
df4 <- as.data.frame(cbind(diff(df2[,1]), diff(df2[,2]), diff(df2[,3]), diff(df2[,4])))
df5 <- as.data.frame(cbind(diff(df4[,1]), diff(df4[,2]), diff(df4[,3]), diff(df4[,4])))

names_y_axis <- c("Mean rainfall", "Mean temp", "Agr. land", "Agr. GDP")

plot_list2 <- lapply(names(df2), function(x)  
  ggplot(df, aes(y = df2[[x]], x = year)) + geom_line(size = 0.5) + theme_bw() +
    ggtitle(names_y_axis[which(names(df2) == x)]) +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) + 
    labs(x = "", y = ""))

plot_list3 <- lapply(names(df3), function(x)  
  ggplot(df, aes(y = df3[[x]], x = year)) + geom_line(size = 0.5) + theme_bw() + 
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) + 
    labs(x = "", y = "")) 

plot_list4 <- lapply(names(df4), function(x)  
  ggplot(df[-56,], aes(y = df4[[x]], x = year)) + geom_line(size = 0.5) + theme_bw() + 
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) + labs(x = "", y = ""))

plot_list5 <- lapply(names(df5), function(x)  
  ggplot(df[-c(55:56),], aes(y = df5[[x]], x = year)) + geom_line(size = 0.5) + theme_bw() + 
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) + labs(x = "", y = "")) 

grid.arrange(grobs=c(plot_list2, plot_list3, plot_list4, plot_list5), ncol=4, nrow = 4)



#plot in part 2

df2$gdp_capita <- df2$NV.AGR.TOTL.KD/df$SP.POP.TOTL
df2$gdp_land <- df2$NV.AGR.TOTL.KD/df2$AG.LND.AGRI.K2

names_y_axis <- c("Mean rainfall", "Mean temp", "Agr. land", "Agr. GDP", "Agr. GDP per capita", "Agr. GDP per square km of agr. land" )

plot_list <- lapply(names(df2), function(x)  
  ggplot(df, aes(y = df2[[x]], x = year)) + geom_line(size = 0.5) + theme_bw() +
    labs(x = "Year", y = names_y_axis[which(names(df2) == x)])) 

grid.arrange(grobs= plot_list, ncol=3, nrow = 2)

## QUESTION 3 ##

rm(list=ls())
dev.off()

# Load libraries
library(urca)
library(tseries)
library(patchwork)
library(LSTS)
library(ggplot2)

#Importing data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
filename_2 <- "VU_MultivariateEcnmtrcs_assignment_dataset.csv"
data <- read.csv(filename_2,header = TRUE)
data <- subset(data, cntry.name == "India")
rownames(data) <- 1:nrow(data)
attach(data)

# Subset data to the variables we are using
GDP_pop <- data.frame(data[,12]/data[,9])
data1 <- cbind.data.frame(data[,1],data[,2],data[,3],data[,6],data[,10],GDP_pop)
colnames(data1) = c('','','mean_pre','mean_tmp','AG.LND.AGRI.K2','GDP_pop')

##  (iii) Conduct unit root tests

# 1. DF test
var1_df <- ur.df(data1[,3], lags = 1, type = c('none'), selectlags = c('BIC')) 
var2_df <- ur.df(data1[,4], lags = 1, type = c('drift'), selectlags = c('BIC'))
var3_df <- ur.df(data1[,5], lags = 1, type = c('trend'), selectlags = c('BIC'))
var4_df <- ur.df(data1[,6], lags = 1, type = c('trend'), selectlags = c('BIC'))


## Test for residuals of DF regression
# Set residuals
var1_res <- data.frame(var1_df@res)
var2_res <- data.frame(var2_df@res)
var3_res <- data.frame(var3_df@res)
var4_res <- data.frame(var4_df@res)

# Plot residuals 
plot(y= t(var1_res), x=1:54, type='l')
plot(y= t(var2_res), x=1:54, type='l')
plot(y= t(var3_res), x=1:54, type='l')
plot(y= t(var4_res), x=1:54, type='l')

# 1. Conduct Ljung box test for the residuals
plot1 <- Box.Ljung.Test(var1_res, lag = 3) + labs(title = "Ljung-Box for average rainfall")
plot2 <- Box.Ljung.Test(var2_res, lag = 3) + labs(title = "Ljung-Box for average temperature")
plot3 <- Box.Ljung.Test(var3_res, lag = 3) + labs(title = "Ljung-Box for average agricultural land")
plot4 <- Box.Ljung.Test(var4_res, lag = 3) + labs(title = "Ljung-Box for agricultural GDP")
# Combine four plots in one figure
plot1 + plot2 + plot3 + plot4

# 2. Correlogram of the residuals
par(mfrow=c(2,2))
par(mar=c(5,5,3,1))
acf(var1_res, main = 'ACF of average rainfall',type = "correlation", plot = TRUE, na.action = na.fail, demean = TRUE)
acf(var2_res, main = 'ACF of average temperature',type = "correlation", plot = TRUE, na.action = na.fail, demean = TRUE)
acf(var3_res, main = 'ACF of agricultural land',type = "correlation", plot = TRUE, na.action = na.fail, demean = TRUE)
acf(var4_res, main = 'ACF of agricultural GDP per capita',type = "correlation", plot = TRUE, na.action = na.fail, demean = TRUE)

# 2. ADF test
var1_adf <- ur.df(data1[,3], lags = 2, type = c('none'), selectlags = c('BIC'))
var2_adf <- ur.df(data1[,4], lags = 4, type = c('trend'), selectlags = c('BIC'))
var3_adf <- ur.df(data1[,5], lags = 4, type = c('trend'), selectlags = c('BIC'))
var4_adf <- ur.df(data1[,6], lags = 10, type = c('trend'), selectlags = c('BIC'))

# 3. Phillips-Perron test
var1 <- PP.test(data1[,3], lshort = TRUE)
var1$p.value # 0.01 - TRUE
var2 <- PP.test(data1[,4], lshort = TRUE)
var2$p.value # 0.01 - TRUE
var3 <- PP.test(data1[,5], lshort = TRUE)
var3$p.value # 0.3019 - FALSE
var4 <- PP.test(data1[,6], lshort = TRUE)
var4$p.value # 0.2462737 - FALSE

## 4. Zivot-Andrews unit root test
var1_za <- ur.za(data1[,3], model = 'both',lag = 4) 
var2_za <- ur.za(data1[,4], model = 'both',lag = 4) 
var3_za <- ur.za(data1[,5], model = 'intercept',lag = 4) 
var4_za <- ur.za(data1[,6], model = 'intercept',lag = 4) 

## 5. KPSS test
var1_kpss <- kpss.test(data[,3],null='Trend',lshort = TRUE) 
var2_kpss <- kpss.test(data[,4],null='Trend',lshort = TRUE) 
var3_kpss <- kpss.test(data[,5],null='Trend',lshort = TRUE) 
var4_kpss <- kpss.test(data[,6],null='Trend',lshort = TRUE) 

## QUESTION 4 ##

rm(list=ls())

library(urca)
library(tseries)
library(cointReg)
library(aTSA)
library(vars)
library(dynlm)
library(stats)
library(ecm)
library(stargazer)
library(ggplot2)
library(lmtest)
library(ggpubr)
library(sandwich)
library(tsDyn)

#Importing data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
filename_2 <- "VU_MultivariateEcnmtrcs_assignment_dataset.csv"
data <- read.csv(filename_2,header = TRUE)
data <- subset(data, cntry.name == "India")
rownames(data) <- 1:nrow(data)
attach(data)

#######################---Question 2---#######################
#Variable transformation
log_agr_gdp_capita <- log(NV.AGR.TOTL.KD/SP.POP.TOTL)
log_agr_gdp_km <- log(NV.AGR.TOTL.KD/AG.LND.AGRI.K2)
rainfall <- mean_pre
temps <- mean_tmp
dataplot <- cbind(rainfall, temps, log_agr_gdp_capita, log_agr_gdp_km)

#Get time series variables
rainfall <- ts(data$mean_pre, start = c(1961, 1, 56), frequency = 1)
temps <- ts(data$mean_tmp, start = c(1961, 1, 56), frequency = 1)
agr_land <- ts(data$AG.LND.AGRI.K2, start = c(1961, 1, 56), frequency = 1)
agr_gdp <- ts(data$NV.AGR.TOTL.KD, start = c(1961, 1, 56), frequency = 1)
log_agr_gdp <- ts(log(agr_gdp), start = c(1961, 1, 56), frequency = 1)
agr_gdp_capita <- ts(agr_gdp/data$SP.POP.TOTL, start = c(1961, 1, 56), frequency = 1)
log_agr_gdp_capita <- ts(log(agr_gdp_capita), start = c(1961, 1, 56), frequency = 1)
agr_gdp_km <- ts(agr_gdp/agr_land, start = c(1961, 1, 56), frequency = 1)
log_agr_gdp_km <- ts(log(agr_gdp_km), start = c(1961, 1, 56), frequency = 1)

#a) Engle-Granger 2-step approach --> critical values: MacKinnon (2010),  table 2

#Some plots
gdpcap_plot = ggplot() + 
  geom_line(data = data, aes(x = year, y = log_agr_gdp_capita), color = "red")+
  xlab("Year")+
  scale_x_continuous(limits=c(1965, 2015))+
  ylab("log Agr. GDP per capita")+ theme(text = element_text(size = 4)) 

temps_plot = ggplot() + 
  geom_line(data = data, aes(x = year, y = temps), color = "blue") +
  xlab("")+
  scale_x_continuous(limits=c(1965, 2015))+
  ylab("Temperature")+ theme(text = element_text(size = 4)) 

gdpkm_plot = ggplot() + 
  geom_line(data = data, aes(x = year, y = log_agr_gdp_km), color = "orange")+
  xlab("")+
  scale_x_continuous(limits=c(1965, 2015))+
  ylab("log Agr. GDP/Agr. land")+ theme(text = element_text(size = 4)) 

rain_plot = ggplot() + 
  geom_line(data = data, aes(x = year, y = rainfall), color = "purple")+
  xlab("")+
  scale_x_continuous(limits=c(1965, 2015))+
  ylab("Rainfall")+ theme(text = element_text(size = 4)) 

ggarrange(temps_plot,
          rain_plot,
          gdpkm_plot,
          gdpcap_plot, ncol=1, nrow=4)



#s1. log GDP per km - rainfall, temps
nlags <- round(nrow(data)^(1/3))
reg1 <- dynlm(log_agr_gdp_km~temps + rainfall)
summary(ur.df(reg1$residuals, type = "none", lags = nlags)) #with rule of thumb lag selection
lagsel <- VARselect(cbind(log_agr_gdp_km, temps, rainfall), lag.max = 7, type = "none")
summary(ur.df(reg1$residuals, type = "none", lags = lagsel[["selection"]][["AIC(n)"]])) #with AIC lags selection
CVs1 <- (-3.74066)+ (-8.5631/nrow(data)) + (-10.852/(nrow(data)^2)) + (27.982/(nrow(data)^3))

#s2. log GDP per capita - rainfall,  temps
reg2 <- dynlm(log_agr_gdp ~ rainfall + temps)
summary(ur.df(reg2$residuals, type = "none", lags = nlags)) #with rule of thumb lag selection
lagsel <- VARselect(cbind(log_agr_gdp, rainfall), lag.max = 7, type = "none")
summary(ur.df(reg2$residuals, type = "none", lags = lagsel[["selection"]][["AIC(n)"]])) #with AIC lags selection
CVs2 <- (-3.74066)+ (-8.5631/nrow(data)) + (-10.852/(nrow(data)^2)) + (27.982/(nrow(data)^3))

#s3. log GDP per km - temps
reg3 <- dynlm(log_agr_gdp_km~temps)
summary(ur.df(reg3$residuals, type = "none", lags = nlags)) #with rule of thumb lag selection
lagsel <- VARselect(cbind(log_agr_gdp_km, temps), lag.max = 7, type = "none")
summary(ur.df(reg3$residuals, type = "none", lags = lagsel[["selection"]][["AIC(n)"]])) #with AIC lags selection
CVs3 <- (-3.33613)+ (-6.1101/nrow(data)) + (-6.823/(nrow(data)^2))

#log GDP per capita - temps
reg4 <- dynlm(log_agr_gdp_capita~temps)
summary(ur.df(reg4$residuals, type = "none", lags = nlags))#with rule of thumb lag selection
lagsel <- VARselect(cbind(log_agr_gdp_capita, temps), lag.max = 7, type = "trend")
summary(ur.df(reg4$residuals, type = "none", lags = lagsel[["selection"]][["AIC(n)"]])) #with AIC lags selection
CVs4 <- (-3.33613)+ (-6.1101/nrow(data)) + (-6.823/(nrow(data)^2))


#b) Phillips-Ouliaris test

#s1. #log GDP per km - rainfall,  temps
po.test(cbind(log_agr_gdp_km, rainfall, temps))

#s2. log GDP per capita - rainfall,  temps
po.test(cbind(log_agr_gdp_capita, rainfall, temps))

#s3. log GDP per km - rainfall,  temps
po.test(cbind(log_agr_gdp_km, temps))

#log GDP per capita - rainfall,  temps, land
po.test(cbind(log_agr_gdp_capita, temps))



#c) Johansen test

#s1. log GDP per km - rainfall, temps
lagsel <- VARselect(cbind(log_agr_gdp_km, rainfall, temps), lag.max = 7, type = "none")
summary(ca.jo(data.frame(log_agr_gdp_km, rainfall, temps), type="trace", K=lagsel[["selection"]][["AIC(n)"]], ecdet="none", spec="longrun")) #with AIC lags
summary(ca.jo(data.frame(log_agr_gdp_km, rainfall, temps), type="trace", K=nlags, ecdet="none", spec="longrun")) #with rule of thumb lags

#s2. log GDP per capita - rainfall, temps
lagsel <- VARselect(cbind(log_agr_gdp_capita, temps, rainfall), lag.max = 7, type = "none")
summary(ca.jo(data.frame(log_agr_gdp_capita, temps, rainfall), type="trace", K=lagsel[["selection"]][["AIC(n)"]], ecdet="none", spec="longrun")) #with AIC lags
summary(ca.jo(data.frame(log_agr_gdp_capita, temps, rainfall), type="trace", K=nlags, ecdet="none", spec="longrun")) #with rule of thumb lags

#log GDP per km - temps
lagsel <- VARselect(cbind(log_agr_gdp_km, temps), lag.max = 7, type = "none")
summary(ca.jo(data.frame(log_agr_gdp_km, temps), type="trace", K=lagsel[["selection"]][["AIC(n)"]], ecdet="none", spec="longrun")) #with AIC lags
summary(ca.jo(data.frame(log_agr_gdp_km, temps), type="trace", K=nlags, ecdet="none", spec="longrun")) #with rule of thumb lags

#s4. log GDP per capita - temps
lagsel <- VARselect(cbind(log_agr_gdp_capita, temps), lag.max = 7, type = "none")
summary(ca.jo(data.frame(log_agr_gdp_capita, temps), type="trace", K=lagsel[["selection"]][["AIC(n)"]], ecdet="none", spec="longrun")) #with AIC lags
summary(ca.jo(data.frame(log_agr_gdp_capita, temps), type="trace", K=nlags, ecdet="none", spec="longrun")) #with rule of thumb lags

#Plotting the estimated relationships using the eigenvectors of the Johansen's test output
s1 = 1*log_agr_gdp_km+0.1682218*rainfall -1.5644286*temps
s2 = 1*log_agr_gdp_capita +0.06480629*rainfall -0.45288818*temps
s3 = 1*log_agr_gdp_capita  -1.37204*temps
s4 = 1*log_agr_gdp_capita -0.001681081 *temps 
p1<-plot(s1, type="l", main="Spec. 1", ylab="")
p2<-plot(s2, type="l", main="Spec. 2", ylab="")
p3<-plot(s3, type="l", main="Spec. 3", ylab="")
p4<-plot(s4, type="l", main="Spec. 4", ylab="")



#######################---Question 4---#######################
#s1. log_agr_gdp_km ~ int. + rainfall + temps
xmat <-  as.data.frame(cbind(rainfall, temps))
statols1 <- lm(log_agr_gdp_km ~ rainfall + temps) #static OLS
summary(statols1)
ress1 <- lm(statols1$residuals[-(nrow(data))]~statols1$residuals[-1])
dwtest(log_agr_gdp_km ~ rainfall + temps) #residuals are autocorrelated
cointReg(method = "FM", xmat, log_agr_gdp_km) #fully modified LS
cointReg(method = "D", xmat, log_agr_gdp_km) #dynamic LS

#ECM: variables definition
D_log_agr_gdp_km <- diff(log_agr_gdp_km)
D_rainfall <- diff(rainfall)
D_temps <- diff(temps)
ecm1 <- dynlm(D_log_agr_gdp_km ~ log_agr_gdp_km[2:nrow(data)] + rainfall[2:nrow(data)] + temps[2:nrow(data)] + D_rainfall + D_temps) #using normal OLS
summary(ecm1)
dwtest(D_log_agr_gdp_km ~ log_agr_gdp_capita[2:nrow(data)] + rainfall[2:nrow(data)] + temps[2:nrow(data)] + D_rainfall + D_temps) #residuals are autocorrelated
summary(ecm(log_agr_gdp_km, xmat, xmat))

#s2. log_agr_gdp_capita ~ rainfall + temps
xmat <-  as.data.frame(cbind(rainfall, temps))
statols2 <- lm(log_agr_gdp_capita ~ rainfall + temps) #static OLS
summary(statols2)
dwtest(log_agr_gdp_capita ~ rainfall + temps) #residuals aren autocorrelated
cointReg(method = "FM", xmat, log_agr_gdp_capita) #fully modified LS
cointReg(method = "D", xmat, log_agr_gdp_capita) #dynamic LS

#ECM: variables definition
D_log_agr_gdp_capita <- diff(log_agr_gdp_capita)
ecm2 <- dynlm(D_log_agr_gdp_capita ~ log_agr_gdp_capita[2:nrow(data)] + rainfall[2:nrow(data)] + temps[2:nrow(data)] + D_rainfall + D_temps) #using normal OLS
summary(ecm2)
dwtest(D_log_agr_gdp_capita ~ log_agr_gdp_capita[2:nrow(data)] + rainfall[2:nrow(data)] + temps[2:nrow(data)] + D_rainfall + D_temps) #residuals aren't autocorrelated
summary(ecm(log_agr_gdp_capita, xmat, xmat)) #using package 

#s3. log_agr_gdp_capita ~ temps
statols3 <- lm(log_agr_gdp_km ~ temps -1) #static OLS
summary(statols3)
dwtest(log_agr_gdp_km ~ temps-1) #residuals are autocorrelated
cointReg(method = "FM", temps, log_agr_gdp_km) #fully modified LS
cointReg(method = "D", temps, log_agr_gdp_km) #dynamic LS

#ECM: variables definition
ecm3 <- dynlm(D_log_agr_gdp_km ~ log_agr_gdp_km[2:nrow(data)] + temps[2:nrow(data)] + D_temps -1) #using normal OLS
summary(ecm3)
dwtest(D_log_agr_gdp_km ~ log_agr_gdp_km[2:nrow(data)] + temps[2:nrow(data)] +  D_temps -1) #residuals aren't autocorrelated

#s4. log_agr_gdp_capita ~ temps
statols4 <- lm(log_agr_gdp_capita ~ temps -1) #static OLS
summary(statols4)
dwtest(log_agr_gdp_capita ~ temps-1) #residuals are autocorrelated
cointReg(method = "FM", temps, log_agr_gdp_capita) #fully modified LS
cointReg(method = "D", temps, log_agr_gdp_capita) #dynamic LS

#ECM: variables definition
ecm4 <- dynlm(D_log_agr_gdp_capita ~ log_agr_gdp_capita[2:nrow(data)] + temps[2:nrow(data)] + D_temps -1) #using normal OLS
summary(ecm3)
dwtest(D_log_agr_gdp_capita ~ log_agr_gdp_capita[2:nrow(data)] + temps[2:nrow(data)] +  D_temps -1) #residuals aren't autocorrelated



#######################---Question 5---#######################

df1 <- data.frame(log_agr_gdp_km, temps, rainfall)
df2 <- data.frame(log_agr_gdp_capita, temps, rainfall)
df3 <- data.frame(log_agr_gdp_km, temps)
df4 <- data.frame(log_agr_gdp_capita, temps)

VARselect(df1, lag.max = 5, type = "none")
VARselect(df2, lag.max = 5, type = "none")
VARselect(df3, lag.max = 5, type = "none")
VARselect(df4, lag.max = 5, type = "none")

summary(ca.jo(df1, type="trace", K=4, ecdet="none", spec="longrun"))
summary(ca.jo(df2, type="trace", K=4, ecdet="none", spec="longrun"))
summary(ca.jo(df3, type="trace", K=4, ecdet="none", spec="longrun"))
summary(ca.jo(df4, type="trace", K=4, ecdet="none", spec="longrun"))

vecm1 <- VECM(df1, lag = 2, r = 1, estim = "ML")
vecm2 <- VECM(df2, lag = 2, r = 1, estim = "ML")
vecm3 <- VECM(df3, lag = 2, r = 1, estim = "ML")
vecm4 <- VECM(df4, lag = 2, r = 1, estim = "ML")

summary(vecm1)
summary(vecm2)
summary(vecm3)
summary(vecm4)

coefB(vecm1)
coefB(vecm2)
coefB(vecm3)
coefB(vecm4)

#Analyze potential changes in the ECT
#Check where the breakpoint in temps is
tmp<-(data$mean_tmp)
fs.tmp<-Fstats(tmp ~ 1)
plot(fs.tmp)
breakpoints(fs.tmp)


df1_2 <- df1[1:36,]
df2_2 <- df2[1:36,]
df3_2 <- df3[1:36,]
df4_2 <- df4[1:36,]

VARselect(df1_2, lag.max = 5, type = "none")
VARselect(df2_2, lag.max = 5, type = "none")
VARselect(df3_2, lag.max = 5, type = "none")
VARselect(df4_2, lag.max = 5, type = "none")

summary(ca.jo(df1_2, type="trace", K=4, ecdet="none", spec="longrun"))
summary(ca.jo(df2_2, type="trace", K=4, ecdet="none", spec="longrun"))
summary(ca.jo(df3_2, type="trace", K=4, ecdet="none", spec="longrun"))
summary(ca.jo(df4_2, type="trace", K=4, ecdet="none", spec="longrun"))

vecm1 <- VECM(df1_2, lag = 2, r = 1, estim = "ML")
vecm2 <- VECM(df2_2, lag = 2, r = 1, estim = "ML")
vecm3 <- VECM(df3_2, lag = 2, r = 1, estim = "ML")
vecm4 <- VECM(df4_2, lag = 2, r = 1, estim = "ML")

summary(vecm1)
summary(vecm2)
summary(vecm3)
summary(vecm4)

coefB(vecm1)
coefB(vecm2)
coefB(vecm3)
coefB(vecm4)



