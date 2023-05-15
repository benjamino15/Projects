df <- read.csv(file = "/Users/benjamin/R/AE/components_47.csv")

ll_points <- matrix(NA, nrow(df), 1)

beta0 <- 1
beta1 <- -0.1
beta2 <- -0.5

theta <- c(beta0, beta1, beta2)

############# Question 1

log_likelihood <- function(df, theta){
  for(i in 1:nrow(df)){
    if(df$Fail.time[i] == "31/12/2021") {
      ll_points[i] <- -exp(theta[1] + theta[2]*df$RQ[i] + theta[3]*df$ATL[i])*df$FTD[i]
  } else{
    ll_points[i] <- theta[1] + theta[2]*df$RQ[i] + theta[3]*df$ATL[i] - 
      exp(theta[1] + theta[2]*df$RQ[i] + theta[3]*df$ATL[i])*df$FTD[i]
  }
 }
  return(ll_points)
}

ll_points <- log_likelihood(df, theta)
total_ll <- sum(ll_points)
avg_ll <- sum(ll_points)/nrow(ll_points)

#Check hint file
theta <- c(1.5, 0.1, 0.1) #New values to check with hint file
ll_points <- log_likelihood(df, theta)
total_ll <- sum(ll_points)
avg_ll <- sum(ll_points)/nrow(ll_points)
#Correct result!

############# Question 2

component_j <- c(names(table(df$Component)))

sum_log_lklh <- function(df, theta){
  ll_points_j <- matrix(NA, nrow(df), 1)
  for(i in 1:nrow(df)){
    if(df$Fail.time[i] == "31/12/2021") {
      ll_points_j[i] <- -exp(theta[1] + theta[2]*df$RQ[i] + theta[3]*df$ATL[i])*df$FTD[i]
    } else{
      ll_points_j[i] <- theta[1] + theta[2]*df$RQ[i] + theta[3]*df$ATL[i] - 
        exp(theta[1] + theta[2]*df$RQ[i] + theta[3]*df$ATL[i])*df$FTD[i]
    }
  }
  return(sum(-ll_points_j)) #Same result for total and avg. likelihood, so I leave it like this
}

result_par <- matrix(NA, nrow = length(component_j), ncol = length(theta))
result_value <- matrix(NA, nrow = length(component_j), ncol = 1)

logmean <- -log(tapply(df$FTD, df$Component, mean))

for(j in component_j){
  result_par[which(component_j == j),] <- optim(par = c(0, 0, logmean[j]), 
                                                fn = sum_log_lklh, 
                                                df = df[df$Component == j,])$par
  result_value[which(component_j == j)] <- -optim(par = c(0, 0, logmean[j]), #negative to get value in neg
                                                  fn = sum_log_lklh, 
                                                  df = df[df$Component == j,])$value
}

result_par
result_value


#Results only for buckle to check with hint file
-optim(par = c(0, 0, logmean["buckle"]), fn = sum_log_lklh, df = df[df$Component == "buckle",])$value
#Correct result!

############# Question 2 extra

#Hessian based

#check in hint file for buckle
hessian <- optim(par = c(0, 0, logmean["buckle"]), fn = sum_log_lklh, 
                 df = df[df$Component == "buckle",], 
                 hessian = TRUE)$hessian

inv_hessian <- solve(hessian)
stand_err <- sqrt(diag(inv_hessian))
stand_err
#Correct result!
#Now do for tire
hessian <- optim(par = c(0, 0, logmean["tire"]), fn = sum_log_lklh, 
                 df = df[df$Component == "tire",], 
                 hessian = TRUE)$hessian

inv_hessian <- solve(hessian)
stand_err <- sqrt(diag(inv_hessian))
stand_err

#OPG-based

library(Deriv)

part_deriv <- matrix(NA, nrow = nrow(df[df$Component == "tire",]), ncol = 3)

for(m in 1:3){
for(i in 1:nrow(df[df$Component == "tire",])){
#Here I create the function for each case separately (without specifying theta, taking them as unknown variables)
func1 <- function(theta1, theta2, theta3) theta1 + theta2*df$RQ[i] + theta3*df$ATL[i] - 
                                         exp(theta1 + theta2*df$RQ[i] + theta3*df$ATL[i])*df$FTD[i]

func2 <- function(theta1, theta2, theta3) -exp(theta1 + theta2*df$RQ[i] + theta3*df$ATL[i])*df$FTD[i]

if(df$Fail.time[i] == "31/12/2021"){
f <- Deriv(func1, paste("theta", m, sep = "")) #Takes the derivative, with respect to beta_m
part_deriv[i,m] <- f(result_par[5,1], result_par[5,2], result_par[5,3]) #Evaluate the part_deriv at the optimal points
} else {
f <- Deriv(func1, paste("theta", m, sep = "")) 
part_deriv[i,m] <- f(result_par[5,1], result_par[5,2], result_par[5,3])
    }
  }
}

part_deriv <- as.matrix(part_deriv)
OPG_matrix <-  t(part_deriv) %*% part_deriv
stand_err_OPG <- solve(OPG_matrix)
#error singular matrix 


############# Question 3
  
df$indicator <- ifelse(df$Fail.time == "31/12/2021", 0, 1)
#theta <- c(beta1, beta2, gamma0, gamma1, gamma2)
theta <-c(-0.1, -0.5, 1, 0.8, 0.025)

filter_fun <- function(df, theta){
  beta0_filter_ts <- matrix(NA, nrow = nrow(df), ncol = 1)
  beta0_filter_ts[1] <- theta[3]
  for(i in 2:nrow(df)){
    beta0_filter_ts[i]<- theta[3]*(1-theta[4]) + theta[4]*beta0_filter_ts[i-1] + 
                         theta[5]*(df$indicator[i-1] -exp(beta0_filter_ts[i-1] +
                          theta[1]*df$RQ[i-1] + theta[2]*df$ATL[i-1])*df$FTD[i-1])
  }
  return(beta0_filter_ts)
}

beta0_filter_ts <- filter_fun(df[df$Component == "tire",], theta)

log_likelihood_filter <- function(df, theta){
  ll_points <- matrix(NA, nrow(df), 1)
  for(i in 1:nrow(df)){
    if(df$Fail.time[i] == "31/12/2021") {
      ll_points[i] <- -exp(beta0_filter_ts[i] + theta[1]*df$RQ[i] + theta[2]*df$ATL[i])*df$FTD[i]
    } else{
      ll_points[i] <- beta0_filter_ts[i] + theta[1]*df$RQ[i] + theta[2]*df$ATL[i] - 
        exp(beta0_filter_ts[i] + theta[1]*df$RQ[i] + theta[2]*df$ATL[i])*df$FTD[i]
    }
  }
  return(sum(ll_points)/length(ll_points))
}
  
log_likelihood_filter(df[df$Component == "tire",], theta)
  
#to test
theta <- c(0.1, 0.1, 1.5, 0.95, 0.01)
#rerun beta0 filter and log likelihood
#Test is correct!

############# Question 4


log_likelihood_filter2 <- function(df, theta, init_beta0){
  ll_points <- matrix(NA, nrow(df), 1)
  beta0_filter_ts <- matrix(NA, nrow = nrow(df), ncol = 1)
  beta0_filter_ts[1] <- theta[3]
  for(i in 1:nrow(df)){
    if(df$Fail.time[i] == "31/12/2021") {
      beta0_filter_ts[i]<- ifelse(i==1, init_beta0, theta[3]*(1-theta[4]) + theta[4]*beta0_filter_ts[i-1] + 
                                                   theta[5]*(df$indicator[i-1] -exp(beta0_filter_ts[i-1] +
                                                   theta[1]*df$RQ[i-1] + theta[2]*df$ATL[i-1])*df$FTD[i-1]))
      
      ll_points[i] <- -exp(beta0_filter_ts[i] + theta[1]*df$RQ[i] + theta[2]*df$ATL[i])*df$FTD[i]
      
    } else{
      beta0_filter_ts[i]<- ifelse(i==1, init_beta0, theta[3]*(1-theta[4]) + theta[4]*beta0_filter_ts[i-1] + 
                                                   theta[5]*(df$indicator[i-1] -exp(beta0_filter_ts[i-1] +
                                                   theta[1]*df$RQ[i-1] + theta[2]*df$ATL[i-1])*df$FTD[i-1]))
      ll_points[i] <- beta0_filter_ts[i] + theta[1]*df$RQ[i] + theta[2]*df$ATL[i] - 
                      exp(beta0_filter_ts[i] + theta[1]*df$RQ[i] + theta[2]*df$ATL[i])*df$FTD[i]
    }
  }
  return(-sum(ll_points))
}

#theta <- c(beta1, beta2, gamma0, gamma1, gamma2)

result_value_dynamic <- matrix(NA, nrow = length(component_j), ncol = 1)
result_par_dynamic <- matrix(NA, nrow = length(component_j), ncol = length(theta))
rownames(result_par) <- component_j

for(j in component_j){
  result_value_dynamic[which(component_j == j),] <- optim(par = c(unname(result_par[j,2]), 
                                                                  unname(result_par[j,3]), 
                                                                  unname(result_par[j,1]),
                                                                  1, 0.025), 
                                                          fn = log_likelihood_filter2, 
                                                          df = df[df$Component == j,],
                                                          init_beta0 = unname(result_par[j,1]))$value
  result_par_dynamic[which(component_j == j),] <- optim(par = c(unname(result_par[j,2]), 
                                                                  unname(result_par[j,3]), 
                                                                  unname(result_par[j,1]),
                                                                  1, 0.025), 
                                                          fn = log_likelihood_filter2, 
                                                          df = df[df$Component == j,],
                                                          init_beta0 = unname(result_par[j,1]))$par
}

result_value_dynamic
result_par_dynamic

#Check hint file for buckle
optim_results <-optim(par = c(unname(result_par["buckle",2]), unname(result_par["buckle",3]), unname(result_par["buckle",1]), 1, 0.025), 
                      fn = log_likelihood_filter2, 
                      df = df[df$Component == "buckle",],
                      init_beta0 = result_par["buckle",1])
optim_results$par
-optim_results$value

#Correct result, by changing the sign of the total log likelihood, but parameters don't change!

############# Question 5

theta <- result_par_dynamic
rownames(theta) <- component_j

filter_fun_j <- function(df, theta){
  beta0_filter_ts_j <- matrix(NA, nrow = max(table(df$Component)), ncol = 5)
  beta0_filter_ts_j[1,] <- t(result_par[,1])
  colnames(beta0_filter_ts_j) <- component_j
  for(j in component_j){
    
  for(i in 2:nrow(df[df$Component == j,])){
    beta0_filter_ts_j[i,j]<- theta[j,3]*(1-theta[j,4]) + theta[j,4]*beta0_filter_ts_j[i-1,j] + 
                             theta[j,5]*(df$indicator[df$Component == j][[i-1]] -exp(beta0_filter_ts_j[i-1,j] +
                             theta[j,1]*df$RQ[df$Component == j][[i-1]] + 
                             theta[j,2]*df$ATL[df$Component == j][[i-1]])*df$FTD[df$Component == j][[i-1]])
    }
  }  
  return(beta0_filter_ts_j)
}
beta0_filter_ts_j <- filter_fun_j(df, theta)
beta0_filter_ts_j <- as.data.frame(beta0_filter_ts_j)


par(mfrow = c(2,3))
for(j in component_j){
plot(na.omit(beta0_filter_ts_j[,j]), type = "l", col ="red", lty = "dashed", 
     ylab = "", main = paste(j, sep = " "))
abline(h=result_par[j,1], lwd = 1.5)
}
mtext("ML estimate for the intercept (black) & the filtered parameter (red)",                  
      side = 3,
      line = - 1.15,
      outer = TRUE)
#export figure and change the width = 800 & height = 600 to have a nicer looking plot

############# Question 6

table <- cbind(-result_value, result_value_dynamic)
rownames(table) <- component_j
table <- as.data.frame(table)
table$diff <- table[,1] - table[,2]
colnames(table) <- c("model Q2", "model Q4", "diff")
print(table)

#Two biggest difference is with casket and hinge

#The results from the figure reflect exactly what is described in the problem statement 
#in section 4 in the assignment. For one component, the manufacturer was switched for 
#some time and the switched back, we see this captured in the abrupt change in 
#the filtered coefficient for the component casket. 

#For the component hinge, we see that the time varying parameter decreases over time
#Which leads to the parameter lambda to decrease, and the mean of the distribution
#to increase (since the mean of the exponential is lambda^-1).This can be interpreted 
#as the quality of the supplier to increase for the component hinge.


