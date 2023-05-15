rm(list=ls())
setwd("C:/Users/Mario/OneDrive - Universidad Carlos III de Madrid/Documentos/VU/Advance econometrics/Assigment 3")

library(gridExtra); library(ggpubr); library(dplyr)
library(readr); library(tidyr); library(psych); library(ggplot2); 
library(hrbrthemes); library(xts)


###Question 1

omega <- 0
alpha <- 0.05
beta <- 0.9
sigma2_lag <- 1

sequence <- seq(from = -2, to = 2, by = 0.001)

sigma2_filter <- function(delta, lambda){
  ts_filter <- c()
  for(i in sequence){
    ts_filter[which(sequence == i)] <- omega + (alpha*i^2 + delta*i^2 *ifelse(i<0 ,1, 0))/(1+(i^2)/(lambda*sigma2_lag)) + 
      beta*sigma2_lag
  }  
  return(ts_filter)
}

lambdas <- c(2, 5, 10, 50)
deltas <- c(0, 0.2, 0.4, 1)

ts_filter_all <- matrix(NA, nrow = 4001, ncol = length(lambdas)*length(deltas))

for(i in 1:length(lambdas)){
  for(j in 1:length(deltas)){
    ts_filter_all[,(i-1)*4 +j] <- sigma2_filter(deltas[j], lambdas[i])
  }
}

ts_filter_all <- as.data.frame(ts_filter_all)

names <- c("delta = 0", "delta = 0.2", "delta = 0.4", "delta = 1")


#for loop to transform wide into long data used for plotting 
for(i in 1:4){
  #separate into four dataframes, one for each subplot
  assign(paste0("filter_lambda", i), ts_filter_all[,seq(from = (i-1)*4 + 1, to = i*4)])
  #transform into long format
  assign(paste0("filter_lambda", i), cbind(gather(get(paste0("filter_lambda", i)), 
                                                  factor_key = TRUE), rep(sequence, 4)))
  
}

colnames(filter_lambda1)[[3]] <- "X"
colnames(filter_lambda2)[[3]] <- "X"
colnames(filter_lambda3)[[3]] <- "X"
colnames(filter_lambda4)[[3]] <- "X"



plot_list <- list()
for(i in 1:4){
  plot_list[[i]] <- ggplot(get(paste0("filter_lambda", i)), aes(x = X, y = value)) + 
    geom_line(aes(color = key), size = 0.5) +
    scale_color_manual(values = c("#82B446", "#7a8596", "#4682B4", "#46B4AF"), 
                       breaks = c(paste0("V",(i-1)*4 + 1),
                                  paste0("V",(i-1)*4 + 2),
                                  paste0("V",(i-1)*4 + 3),
                                  paste0("V",(i-1)*4 + 4)),
                       labels = c(paste(expression("\u03B4 ="), deltas[1]),
                                  paste(expression("\u03B4 ="), deltas[2]),
                                  paste(expression("\u03B4 ="), deltas[3]),
                                  paste(expression("\u03B4 ="), deltas[4]))) +
    labs(y = expression(~sigma[t]^2), x = expression(~x[t]), 
         title=paste(expression("\u03BB ="), lambdas[i])) +
    theme_bw() + ylim(0.75, 5)
}
grid.arrange(grobs=plot_list,ncol=2, nrow = 2)

###Question 2

##Upload variables
#JNJ

JNJ <- read.csv("JNJ.csv", header = T, sep = ",", dec = ".")

#MKM

MRK <- read.csv("MRK.csv", header = T, sep = ",", dec = ".")

#PFE

PFE <- read.csv("PFE.csv", header = T, sep = ",", dec = ".")

#KO

KO <- read.csv("KO.csv", header = T, sep = ",", dec = ".")

##Combine of all them
# Generate a variable only with date that it is equal to the variable JNJ
# This variables date I want to be a data.frame
DATE = KO$date
DATE = data.frame(DATE)

demeanKO = (KO$RET - mean(KO$RET))*100
demeanJNJ = (JNJ$RET - mean(JNJ$RET))*100
demeanMRK = (MRK$RET - mean(MRK$RET))*100
demeanPFE = (PFE$RET - mean(PFE$RET))*100

#Create data.frame
X1 =  data.frame(DATE, demeanKO, demeanJNJ, demeanMRK, demeanPFE)
names(X1)[c(1,2,3,4,5)]= c("DATE","KO","JNJ","MRK","PFE")


#Set a Time series

dates <- seq(as.Date("2001/01/02"), length = 5284, by = "days")
X <- xts(x = X1, order.by = dates)

write.csv(X, file = "Finaldata.csv")

##Plots Q2

#All data together in long format

rawdata <- read.csv("stocks2.csv", header = T, sep = ",", dec = ".")

##Transform the data
#Substracting the sample mean

#Mean 

meanrawdata <- rawdata %>% group_by(TICKER) %>% summarize(avg = mean(RET))

#Create new dataframe
#Add the data to the mean

data <- rawdata %>% group_by(TICKER) %>% mutate(avg = mean(RET))

#Substract the mean and multiply by 100

data <- data %>% group_by(TICKER) %>% mutate(demean = (RET - avg)*100)

#Remove variables

data <- data[ -c(1,4:5) ]

##Question2

#Check mean is the same

meandata <- data %>% group_by(TICKER) %>% summarize(avg = mean(demean))

#Descriptive stats

Q2 <- data %>% group_by(TICKER) %>% summarize(describe(demean))

Q2 <- Q2[ -c(2,7:8,11,14)]

Q2 <- t(Q2)

#Export table

write.csv(Q2, file = "Tabl2F.csv")

#Rename variables

colnames(data) = c("Dates", "Stock", "Returns")

#Plot

KOplot <- filter(data, Stock == "KO")
ggplot(KOplot,                            
       aes(x = Dates,
           y = Returns,
           col = Stock,
           group= Stock)) +
  geom_line()+
  theme_ipsum()

JNJplot <- filter(data, Stock == "JNJ")
ggplot(JNJplot,                            
       aes(x = Dates,
           y = Returns,
           col = Stock,
           group= Stock)) +
  geom_line()+
  theme_ipsum()

MRKplot <- filter(data, Stock == "MRK")
ggplot(MRKplot,                            
       aes(x = Dates,
           y = Returns,
           col = Stock,
           group= Stock)) +
  geom_line()+
  theme_ipsum()

PFEplot <- filter(data, Stock == "PFE")
ggplot(PFEplot,                            
       aes(x = Dates,
           y = Returns,
           col = Stock,
           group= Stock)) +
  geom_line()+
  theme_ipsum()


#### Question 3

#Read again the data, should be the same of X but idk why it does not work with X

df <- read.csv("Finaldata.csv")

#MODEL WITHOUT LEVERAGE EFFECT (DELTA ==0) 
Logl_no_leverage <- function(theta,x){
  omega <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  delta <- 0
  lambda <- theta[4]
  #logl_v <- vector(mode ='numeric', length = nrow(ass3_finaldata))
  #sigma2 <- vector(mode ='numeric', length = nrow(ass3_finaldata))
  logl_v <- rep(0,2501)
  sigma2 <- rep(0,2501)
  #var(x[1:50])*(49/50)
  sigma2[1]<- var(x[1:50])*(49/50)
  
  for(t in 1:2500){
    sigma2[t+1] <- omega + (alpha*x[t]^2 + delta*x[t]^2 *ifelse(x[t]<0 ,1, 0))/(1+(x[t]^2/(lambda*sigma2[t]))) + beta*sigma2[t]
    logl_v[t] <- lgamma((lambda+1)/2)-lgamma(lambda/2)-(1/2)*log(lambda*pi)-((lambda+1)/2)*log(1+((x[t]^2)/(lambda*sigma2[t])))-(1/2)*log(sigma2[t])
  }  
  return(-sum(logl_v))
}

stocks_name <- c('KO','JNJ','MRK','PFE')
theta <- c("omega","alpha","beta","lambda")

#Parameters result model without leverage 
result_par_no_leverage <- matrix(NA, nrow = length(theta), ncol =length(stocks_name))
colnames(result_par_no_leverage) <- stocks_name
rownames(result_par_no_leverage) <- theta

#Hessian result model without leverage 
result_hessian_no_leverage <- matrix(NA, nrow = 4, ncol =length(stocks_name))
colnames(result_par_no_leverage) <- stocks_name

#Unique optim cycle
for(j in stocks_name){
  aux <- optim(fn = Logl_no_leverage,
  par=c(var(df[[j]])/50, 0.02, 0.96,5), 
  hessian=TRUE,
  x = df[[j]])
  result_par_no_leverage[,which(stocks_name == j)] = aux$par
  result_hessian_no_leverage[,which(stocks_name == j)] <- sqrt(diag(solve(aux$hessian)))
}

#Total Log likelihood model without leverage 
result_llh_no_leverage <- vector(mode ='numeric', length = length(stocks_name))

for(j in stocks_name){
  new_theta <- c(result_par_no_leverage[1,which(stocks_name == j)],result_par_no_leverage[2,which(stocks_name == j)],result_par_no_leverage[3,which(stocks_name == j)],result_par_no_leverage[4,which(stocks_name == j)])
  result_llh_no_leverage[which(stocks_name == j)]<- Logl_no_leverage(theta=new_theta,x=df[[j]])
}

#MODEL WITH LEVERAGE EFFECT (DELTA !=0) 
Logl_leverage <- function(theta,x){
  omega <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  delta <- theta[4]
  lambda <- theta[5]
  logl_v <- rep(0,2501)
  sigma2 <- rep(0,2501)
  sigma2[1]<- var(x[1:50])*(49/50)
  
  for(t in 1:2500){
    sigma2[t+1] <- omega + (alpha*x[t]^2 + delta*x[t]^2 *ifelse(x[t]<0 ,1, 0))/(1+(x[t]^2/(lambda*sigma2[t]))) + beta*sigma2[t]
    logl_v[t] <- lgamma((lambda+1)/2)-lgamma(lambda/2)-(1/2)*log(lambda*pi)-(lambda+1)/2*log(1+((x[t]^2)/(lambda*sigma2[t])))-(1/2)*log(sigma2[t])
  }
                                                            
  
  return(-sum(logl_v))
}


theta <- c("omega","alpha","beta","delta","lambda")

#Parameters result model with leverage 
result_par_leverage <- matrix(NA, nrow = length(theta), ncol =length(stocks_name))
colnames(result_par_leverage) <- stocks_name
rownames(result_par_leverage) <- theta

#Hessian result model with leverage 
result_hessian_leverage <- matrix(NA, nrow = length(theta), ncol =length(stocks_name))
colnames(result_hessian_leverage) <- stocks_name

#Unique optim cycle 
for(j in stocks_name){
  aux <- optim(fn=Logl_leverage,
               par=c(var(df[[j]])/50, 0.02, 0.96,0,5),
               hessian=TRUE,
               x = df[[j]])
  result_par_leverage[,which(stocks_name == j)] = aux$par
  result_hessian_leverage[,which(stocks_name == j)] <- sqrt(diag(solve(aux$hessian)))
}



#Total Log likelihood model with leverage 
result_llh_leverage <- vector(mode ='numeric', length = length(stocks_name))

for(j in stocks_name){
  new_theta <- c(result_par_leverage[1,which(stocks_name == j)],result_par_leverage[2,which(stocks_name == j)],result_par_leverage[3,which(stocks_name == j)],result_par_leverage[4,which(stocks_name == j)],result_par_leverage[5,which(stocks_name == j)])
  result_llh_leverage[which(stocks_name == j)]<- Logl_leverage(theta=new_theta,x=df[[j]])
}

#AIC & BIC 
AIC_BIC <- matrix(NA,4,length(stocks_name))
colnames(AIC_BIC) <- stocks_name
rownames(AIC_BIC) <- c("AIC_NoLeverage","BIC_NoLeverage","AIC_Leverage","BIC_Leverage")

for ( i in 1:2) {
  if(i==1){
    for(j in stocks_name){
      new_theta <- c(result_par_no_leverage[1,which(stocks_name == j)],result_par_no_leverage[2,which(stocks_name == j)],result_par_no_leverage[3,which(stocks_name == j)],result_par_no_leverage[4,which(stocks_name == j)]) 
      AIC_BIC[i,which(stocks_name == j)] <- -2*(Logl_no_leverage(theta=new_theta,x=df[[j]]))+ 2*length(new_theta)
      AIC_BIC[i+1,which(stocks_name == j)] <- -2*(Logl_no_leverage(theta=new_theta,x=df[[j]]))+ length(new_theta)*log(2500)
    }
  }else{
    for(j in stocks_name){
      new_theta <- c(result_par_leverage[1,which(stocks_name == j)],result_par_leverage[2,which(stocks_name == j)],result_par_leverage[3,which(stocks_name == j)],result_par_leverage[4,which(stocks_name == j)],result_par_leverage[5,which(stocks_name == j)]) 
      AIC_BIC[i+1,which(stocks_name == j)] <- -2*(Logl_leverage(theta=new_theta,x=df[[j]]))+ 2*length(new_theta)
      AIC_BIC[i+2,which(stocks_name == j)] <- -2*(Logl_leverage(theta=new_theta,x=df[[j]]))+ length(new_theta)*log(2500)}
  }
}


#### QUESTION 4 ####

df_order_JNJ <-  df$JNJ[order(df$JNJ)] 
df_order_MRK <-  df$MRK[order(df$MRK)]
df_order_PFE <-  df$PFE[order(df$PFE)]
comp_names <- c("JNJ", "MRK", "PFE")
df_JNJ <-  df$JNJ 
df_MRK <-  df$MRK
df_PFE <-  df$PFE


#Change function to allow all parameters to be an input, as well as specifying x
sigma2_filterQ4 <- function(theta, x){
  omega <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  delta <- theta[4]
  lambda <- theta[5]
  
  ts_filter <- c()
  for(i in x){
    ts_filter[which(x == i)] <- omega + (alpha*i^2 + delta*i^2 *ifelse(i<0 ,1, 0))/(1+(i^2)/(lambda*sigma2_lag)) + 
      beta*sigma2_lag
  }  
  return(ts_filter)
}

######### Calculations model with leverage effect

NIC_leverage <- matrix(NA, nrow = nrow(df), ncol = 3)
colnames(NIC_leverage) <- comp_names

filter_leverage <- matrix(NA, nrow = nrow(df), ncol = 3)
colnames(filter_leverage) <- comp_names

for(i in comp_names){
  #News impact curve for model with leverage effect
  NIC_leverage[, i] <- sigma2_filterQ4(get(paste0("theta_", i)), get(paste0("df_order_", i)))
  #Filter for model with leverage effect
  filter_leverage[, i] <- sigma2_filterQ4(get(paste0("theta_", i)), get(paste0("df_", i)))
}

######### Calculations for model without leverage effect

NIC_noleverage <- matrix(NA, nrow = nrow(df), ncol = 3)
colnames(NIC_noleverage) <- comp_names

filter_noleverage <- matrix(NA, nrow = nrow(df), ncol = 3)
colnames(filter_noleverage) <- comp_names

for(i in comp_names){
  #Add a 0 in the fourth element of the vector
  assign(paste0("theta_", i, 0), append(get(paste0("theta_", i, 0)), 0, after = 3))
  #News impact curves for model without leverage effect 
  NIC_noleverage[, i] <- sigma2_filterQ4(get(paste0("theta_", i, 0)), get(paste0("df_order_", i)))
  #Filter for model without leverage effect
  filter_noleverage[, i] <- sigma2_filterQ4(get(paste0("theta_", i, 0)), get(paste0("df_", i)))
}



df_NIC_JNJ <- as.data.frame(cbind(NIC_leverage[,"JNJ"], NIC_noleverage[,"JNJ"]))
df_NIC_MRK <- as.data.frame(cbind(NIC_leverage[,"MRK"], NIC_noleverage[,"MRK"]))
df_NIC_PFE <- as.data.frame(cbind(NIC_leverage[,"PFE"], NIC_noleverage[,"PFE"]))

df_filter_JNJ <- as.data.frame(cbind(filter_leverage[,"JNJ"], filter_noleverage[,"JNJ"]))
df_filter_MRK <- as.data.frame(cbind(filter_leverage[,"MRK"], filter_noleverage[,"MRK"]))
df_filter_PFE <- as.data.frame(cbind(filter_leverage[,"PFE"], filter_noleverage[,"PFE"]))

#For loop to transform data from wide to long format
#
for(i in comp_names){
  #For news impact curves
  assign(paste0("df_NIC_", i), cbind(gather(get(paste0("df_NIC_", i)), factor_key = TRUE), 
                                     rep(df_order_JNJ,2)))
  #For filter 
  assign(paste0("df_filter_", i), cbind(gather(get(paste0("df_filter_", i)), factor_key = TRUE), 
                                        rep(df$DATE, 2)))
  
}

colnames(df_NIC_JNJ)[3] <- "X"
colnames(df_NIC_MRK)[3] <- "X"
colnames(df_NIC_PFE)[3] <- "X"

colnames(df_filter_JNJ)[3] <- "X"
colnames(df_filter_MRK)[3] <- "X"
colnames(df_filter_PFE)[3] <- "X"

df_filter_JNJ$X <- as.Date(df_filter_JNJ$X, format = "%Y/%m/%d")
df_filter_MRK$X <- as.Date(df_filter_MRK$X, format = "%Y/%m/%d")
df_filter_PFE$X <- as.Date(df_filter_PFE$X, format = "%Y/%m/%d")

library(gridExtra)
plot_list <- list()
for(i in comp_names){
  plot_list[[which(comp_names == i)*2 -1]] <- ggplot(get(paste0("df_NIC_", i)), aes(x = X, y = value)) + 
    geom_line(aes(color = key), size = 0.5) +
    scale_color_manual(values = c("#5F4B8BFF", "#E69A8DFF"), 
                       breaks = c("V1", "V2"),
                       labels = c(paste(expression("\u03B4 \U2260"), 0),
                                  paste(expression("\u03B4 ="), 0))) +
    labs(y = expression(~sigma[t]^2), x = expression(~x[t]), 
         title=paste("News impact curve", i)) + theme_bw() 
  
  plot_list[[which(comp_names == i)*2]] <- ggplot(get(paste0("df_filter_", i)), aes(x = X, y = value)) + 
    geom_line(aes(color = key, group = 1), size = 0.5) +
    scale_color_manual(values = c("#5F4B8BFF", "#E69A8DFF"), 
                       breaks = c("V1", "V2"),
                       labels = c(paste(expression("\u03B4 \U2260"), 0),
                                  paste(expression("\u03B4 ="), 0))) +
    labs(y = expression(~sigma[t]^2), x = expression(~x[t]), 
         title=paste("Time-varying filter for", i)) + theme_bw() + 
    geom_vline(xintercept=as.numeric(df_filter_JNJ$X[2500]), linetype=4)
  
}
grid.arrange(grobs=plot_list,ncol=2, nrow = 3)

#### QUESTION 5 ####


#The function 
df_q5 <- df[3:6]

fcast <- function (theta,x){
  
  # Initializing matrix of resutls ##  
  res <- matrix(0,nrow=20,ncol=3)
  colnames(res) = c("vol","ret","shock")
  
  ## Generating initial volatility using MLE estimates (until 21/4/1) ##
  omega <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  
  if(length(theta)==5){
  delta <- theta[4]
  lambda <- theta[5]
  }else{
    lambda <- theta[4]
    delta <- 0
  }
  ss <- rep(0,4843)
  ss[1]<- var(x[1:50])*(49/50)
  
  #We calculate the value of sigma square (the filter) for the first 2500 values 
  for(t in 1:4842){
    ss[t+1] <- omega + (alpha*x[t]^2 + delta*x[t]^2 *ifelse(x[t]<0 ,1, 0))/(1+(x[t]^2/(lambda*ss[t]))) + beta*ss[t]
  }
  ### Setting initial conditions 
  #Volatility(ss)
  res[1,1] = ss[4843]
  #Random innovations (e) from student t, degreess of fredom lambda 
  res[1,3] = rt(1,lambda)
  #Returns: as a multiplication of the two
  res[1,2] = sqrt(res[1,1])*res[1,3]
  
  ## updating the forecast with extraction ##
  for (t in 2:20){
    t1=t-1
    res[t,3] <- rt(1,lambda)
    res[t,1] <- omega + (alpha*res[t1,2]^2 + delta*res[t1,2]^2 *ifelse(res[t1,2]<0 ,1, 0))/(1+(res[t1,2]^2/(lambda*res[t1,1]))) + beta*res[t1,1]
    res[t,2] <- sqrt(res[t,1])*res[t,3]
  }
  
  return(res[,2])
}

## Individual returns
#S: number of simulations
Simulation <- function(par_results,data){
S = 15000; ress_le = matrix(0,S,20)
periods_ahead <- c(1:20)
colnames(ress_le) = periods_ahead

  for (i in periods_ahead){
    for (s in 1:S){
    aux <- fcast(par_results,data)
    ress_le[s,i]=aux[i]
    }
  }
  comp_ret_le = matrix(0,S,3); colnames(comp_ret_le) = c("one","five","twenty")
  comp_ret_le[,1] = ress_le[,1]
  comp_ret_le[,2] = 100*(((1+ress_le[,1]/100)*(1+ress_le[,2]/100)*(1+ress_le[,3]/100)*(1+ress_le[,4]/100)*(1+ress_le[,5]/100))-1)
  comp_ret_le[,3] = 100*(((1+ress_le[,1]/100)*(1+ress_le[,2]/100)*(1+ress_le[,3]/100)*(1+ress_le[,4]/100)*(1+ress_le[,5]/100)*(1+ress_le[,6]/100)*(1+ress_le[,7]/100)*(1+ress_le[,8]/100)*(1+ress_le[,9]/100)*(1+ress_le[,10]/100)*(1+ress_le[,11]/100)*(1+ress_le[,12]/100)*(1+ress_le[,13]/100)*(1+ress_le[,14]/100)*(1+ress_le[,15]/100)*(1+ress_le[,16]/100)*(1+ress_le[,17]/100)*(1+ress_le[,18]/100)*(1+ress_le[,19]/100)*(1+ress_le[,20]/100))-1)
  result_matrix <- matrix(1:9, nrow = 3, dimnames = list(c("h=1","h=5","h=20"),c("0.01","0.05","0.1")))
 
  for(z in 1:3){
    result_matrix[z,] <- quantile(comp_ret_le[,z],probs=c(0.01,0.05,0.1))
  }
  return(t(result_matrix))
   }


#Models for each stock without leverage
 q5_nl_KO <- Simulation(result_par_no_leverage[,1],df_q5[,1])
 q5_nl_JNJ <- Simulation(result_par_no_leverage[,2],df_q5[,2])
 q5_nl_MRK <- Simulation(result_par_no_leverage[,3],df_q5[,3])
 q5_nl_PFE <- Simulation(result_par_no_leverage[,4],df_q5[,4])
 
 #Models for each stock with leverage
 q5_l_KO <- Simulation(result_par_leverage[,1],df_q5[,1])
 q5_l_JNJ <- Simulation(result_par_leverage[,2],df_q5[,2])
 q5_l_MRK <- Simulation(result_par_leverage[,3],df_q5[,3])
 q5_l_PFE <- Simulation(result_par_leverage[,4],df_q5[,4])

result_matrix_nl <- cbind(q5_nl_KO,q5_nl_JNJ,q5_nl_MRK,q5_nl_PFE)
result_matrix_l <- cbind(q5_l_KO,q5_l_JNJ,q5_l_MRK,q5_l_PFE)
q5_result_matrix <- rbind(result_matrix_nl,result_matrix_l)

### QUESTION 6 ###

#Re-state function for the filter with lag of sigma (I've used this funtion for Q4 as well)
sigma2_filterQ5 <- function(theta, x){
  omega <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  delta <- theta[4]
  lambda <- theta[5]
  
  ts_filter <- matrix(NA, nrow = 5284, ncol = 1)
  ts_filter[1] <- var(x[1:50])*(49/50)
  for(i in 2:length(x)){
    ts_filter[i] <- omega + (alpha*x[i-1]^2 + delta*x[i-1]^2 *ifelse(x[i-1]<0 ,1, 0))/(1+(x[i-1]^2)/(lambda*ts_filter[i-1])) + 
      beta*ts_filter[i-1]
  }  
  return(ts_filter)
}

sigma2_filter <- sigma2_filterQ5(theta_coke, df_coke)
#Forecast
est_window <- 2500 #Estimation window
test_window <- nrow(df) - est_window #test window

hit_sequence <- matrix(NA, nrow = (test_window-1), ncol = 1)

for(i in (est_window+1):nrow(df)){
  hit_sequence[i-est_window]<- ifelse(sqrt(sigma2_filter[i])*qt(0.1, theta_coke[5])>df_coke[i],1,0)
}

hit_rate <- sum(hit_sequence)/length(hit_sequence)
hit_rate


gamma <- c(0.01, 0.05, 0.1)

#Hit sequence calculation for JNJ
sigma2_filter <- sigma2_filterQ5(theta_JNJ, df_JNJ)
sigma2_filter0 <- sigma2_filterQ5(theta_JNJ0, df_JNJ)
hit_sequenceJNJ <- matrix(NA, nrow = (test_window), ncol = 6)

for(j in gamma){
  for(i in (est_window+1):nrow(df)){
    
    hit_sequenceJNJ[i-est_window, which(gamma == j)*2 -1]<- ifelse(sqrt(sigma2_filter[i])*qt(j,theta_JNJ[5]) >df_JNJ[i],1,0)
    hit_sequenceJNJ[i-est_window, which(gamma == j)*2]<- ifelse(sqrt(sigma2_filter0[i])*qt(j,theta_JNJ0[5]) >df_JNJ[i],1,0)
  }
}

#Hit sequence calculation for MRK
sigma2_filter <- sigma2_filterQ5(theta_MRK, df_MRK)
sigma2_filter0 <- sigma2_filterQ5(theta_MRK0, df_MRK)
hit_sequenceMRK <- matrix(NA, nrow = (test_window), ncol = 6)

for(j in gamma){
  for(i in (est_window+1):nrow(df)){
    hit_sequenceMRK[i-est_window, which(gamma == j)*2 -1]<- ifelse(sqrt(sigma2_filter[i])*qt(j,theta_MRK[5]) >df_MRK[i],1,0)
    hit_sequenceMRK[i-est_window, which(gamma == j)*2]<- ifelse(sqrt(sigma2_filter0[i])*qt(j,theta_MRK0[5]) >df_MRK[i],1,0)
  }
}

#Hit sequence calculation for  PFE
sigma2_filter <- sigma2_filterQ5(theta_PFE, df_PFE)
sigma2_filter0 <- sigma2_filterQ5(theta_PFE0, df_PFE)
hit_sequencePFE <- matrix(NA, nrow = (test_window), ncol = 6)

for(j in gamma){
  for(i in (est_window+1):nrow(df)){
    hit_sequencePFE[i-est_window, which(gamma == j)*2 -1]<- ifelse(sqrt(sigma2_filter[i])*qt(j,theta_PFE[5]) >df_PFE[i],1,0)
    hit_sequencePFE[i-est_window, which(gamma == j)*2]<- ifelse(sqrt(sigma2_filter0[i])*qt(j,theta_PFE0[5]) >df_PFE[i],1,0)
  }
}
#Results: column 1: leverage 1%, column 2: no leverage 1%, 
#         column 3: leverage 5%, column 4: no leverage 5%,
#         column 5: leverage 10%, column 6: no leverage 10%

apply(hit_sequenceJNJ, 2, sum)/nrow(hit_sequenceJNJ)
apply(hit_sequenceMRK, 2, sum)/nrow(hit_sequenceMRK)
apply(hit_sequencePFE, 2, sum)/nrow(hit_sequencePFE)

#Results for coke
sigma2_filter <- sigma2_filterQ5(theta_coke, df_coke)
hit_sequencecoke <- matrix(NA, nrow = (test_window), ncol = 3)
for(j in gamma){
  for(i in (est_window+1):nrow(df)){
    hit_sequencecoke[i-est_window, which(gamma == j)]<- ifelse(sqrt(sigma2_filter[i])*qt(j,theta_coke[5]) >df_coke[i],1,0)
  }
}
apply(hit_sequencecoke, 2, sum)/(nrow(hit_sequencecoke))

############# Standard Errors!!!!

#Normal Standard errors
std.err <- function(x){
  H <- nrow(x)
  return(apply(x, 2, sd)/sqrt(H))
}

#Newey-West Standard errors
std.err.NW <- function(x){
  
  H <- nrow(x)
  L <- as.integer(H^(1/5))
  results <- matrix(NA, nrow = 1, ncol = 6)
  
  summation <- matrix(NA, nrow = H, ncol = L)
  i = 0
  repeat
  {
    i = i + 1
    
    for(l in 1:L){
      wl <- 1-(l/(L+1))
      for(h in (l+1):H){
        summation[h,l] <- wl*x[h,i]*x[h-l,i]
        
      }
    }
    results[,i] <- sqrt((1/H)*sum(x[,i]^2) + (2/H)*sum(summation, na.rm = TRUE))/sqrt(H)
    
    
    if(i>5)
    {
      break
    }
  }
  return(results)
}


#Results
std.err(hit_sequenceJNJ)
std.err(hit_sequenceMRK)
std.err(hit_sequencePFE)

std.err.NW(hit_sequenceJNJ)
std.err.NW(hit_sequenceMRK)
std.err.NW(hit_sequencePFE)

