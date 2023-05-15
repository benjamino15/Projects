airports <- read.csv(file = "/Users/benjamin/R/Thesis/final_data.csv")
setwd("/Users/benjamin/R/Thesis")
library(dplyr)
library(Synth)
library(SCtools)
airports <- select(airports, -X)
#Order the dataset so that the last units are the treated ones
airports_treated <- filter(airports, airport == "AMSTERDAM/SCHIPHOL airport"| 
                           airport == "EINDHOVEN airport"| airport == "GRONINGEN/EELDE airport" |
                           airport == "MAASTRICHT/AACHEN airport" | airport == "ROTTERDAM airport" |
                           airport == "ANTWERPEN/DEURNE airport" | airport == "BRUSSELS airport" |
                           airport == "CHARLEROI/BRUSSELS SOUTH airport" | airport == "LIEGE airport"|
                           airport == "DUESSELDORF airport"| airport == "NIEDERRHEIN airport"|
                           airport == "MUENSTER/OSNABRUECK airport" | airport == "KOELN/BONN airport")
airports <-filter(airports, !airport == "AMSTERDAM/SCHIPHOL airport", 
                  !airport == "EINDHOVEN airport", !airport == "GRONINGEN/EELDE airport",
                  !airport == "MAASTRICHT/AACHEN airport", !airport == "ROTTERDAM airport",
                  !airport == "ANTWERPEN/DEURNE airport", !airport == "BRUSSELS airport",
                  !airport == "CHARLEROI/BRUSSELS SOUTH airport", !airport == "LIEGE airport",
                  !airport == "DUESSELDORF airport", !airport == "NIEDERRHEIN airport",
                  !airport == "MUENSTER/OSNABRUECK airport", !airport == "KOELN/BONN airport")
#I create another varriable fir time from 1 to 30 because the SC does not read monthly data properly
airports <- rbind(airports, airports_treated)
air_number <- rep(c(1:228), each = 30)
airports$air_number <- air_number
time3 <- rep(seq(1:30), times = 228)
airports <- cbind(airports, time3)
airports <- select(airports, air_number, airport, country, time, time2, time3, 
                   pax, prod_industr_agg, tick_pr, oil_import_pr_mon)
write.csv(airports, file = "/Users/benjamin/R/final_data.csv")
#Data is now filtered so that treated units are at the bottom.
#1:215 are the untreated, while 216:228 are the treated.

#Here I create all the counterfactuals. 1 refers to specification 1 and 2 refers
#to specification 2. 1.1 is spec 1 with restricted data, and 2.1 is spec 2 with 
#restricted data

multiple.synth.out1 <- multiple.synth(foo = airports,
               predictors = c("pax", "prod_industr_agg", "tick_pr"),
               predictors.op = "mean",
               special.predictors = list(list("oil_import_pr_mon", 1:18, "mean")),
               time.predictors.prior = 1:18,
               dependent = "pax",
               unit.variable = "air_number",
               unit.names.variable = "airport",
               time.variable = "time3",
               treated.units = 216:228,
               control.units = 1:215,
               time.optimize.ssr = 1:18,
               treatment.time = 19,
               time.plot = 1:30)
multiple.synth.out2 <- multiple.synth(foo = airports,
               predictors = c("prod_industr_agg", "tick_pr", "oil_import_pr_mon"),
               predictors.op = "mean",
               special.predictors = list(list("pax", 18, "mean")),
               time.predictors.prior = 1:18,
               dependent = "pax",
               unit.variable = "air_number",
               unit.names.variable = "airport",
               time.variable = "time3",
               treated.units = 216:228,
               control.units = 1:215,
               time.optimize.ssr = 1:18,
               treatment.time = 19,
               time.plot = 1:30)

multiple.synth.out11.type2 <- multiple.synth(foo = type2,
                   predictors = c("pax", "prod_industr_agg", "tick_pr"),
                   predictors.op = "mean",
                   special.predictors = list(list("oil_import_pr_mon", 1:18, "mean")),
                   time.predictors.prior = 1:18,
                   dependent = "pax",
                   unit.variable = "air_number",
                   unit.names.variable = "airport",
                   time.variable = "time3",
                   treated.units = 13:14,
                   control.units = 1:12,
                   time.optimize.ssr = 1:18,
                   treatment.time = 19,
                   time.plot = 1:30)

multiple.synth.out11.type3 <- multiple.synth(foo = type3,
                                             predictors = c("pax", "prod_industr_agg", "tick_pr"),
                                             predictors.op = "mean",
                                             special.predictors = list(list("oil_import_pr_mon", 1:18, "mean")),
                                             time.predictors.prior = 1:18,
                                             dependent = "pax",
                                             unit.variable = "air_number",
                                             unit.names.variable = "airport",
                                             time.variable = "time3",
                                             treated.units = 26:27,
                                             control.units = 1:25,
                                             time.optimize.ssr = 1:18,
                                             treatment.time = 19,
                                             time.plot = 1:30)

multiple.synth.out11.type4 <- multiple.synth(foo = type4,
                                             predictors = c("pax", "prod_industr_agg", "tick_pr"),
                                             predictors.op = "mean",
                                             special.predictors = list(list("oil_import_pr_mon", 1:18, "mean")),
                                             time.predictors.prior = 1:18,
                                             dependent = "pax",
                                             unit.variable = "air_number",
                                             unit.names.variable = "airport",
                                             time.variable = "time3",
                                             treated.units = 25:32,
                                             control.units = 1:24,
                                             time.optimize.ssr = 1:18,
                                             treatment.time = 19,
                                             time.plot = 1:30)


dataprepoutAMS11 <- dataprep(foo = type1,
                            predictors = c("pax", "prod_industr_agg", "tick_pr", "oil_import_pr_mon"),
                            predictors.op = "mean",
                            time.predictors.prior = 1:18,
                            dependent = "pax",
                            unit.variable = "air_number",
                            unit.names.variable = "airport",
                            time.variable = "time3",
                            treatment.identifier = 7,
                            controls.identifier = 1:6,
                            time.optimize.ssr = 1:18,
                            time.plot = 1:30)

dataprepoutAMS21 <- dataprep(foo = type1,
                             predictors = c("prod_industr_agg", "tick_pr", "oil_import_pr_mon"),
                             predictors.op = "mean",
                             time.predictors.prior = 1:18,
                             special.predictors = list(list("pax", 18, "mean")),
                             dependent = "pax",
                             unit.variable = "air_number",
                             unit.names.variable = "airport",
                             time.variable = "time3",
                             treatment.identifier = 7,
                             controls.identifier = 1:6,
                             time.optimize.ssr = 1:18,
                             time.plot = 1:30)
synth.outAMS21 <- synth(dataprepoutAMS21)

multiple.synth.out21.type2 <- multiple.synth(foo = type2,
                                             predictors = c( "prod_industr_agg", "tick_pr", "oil_import_pr_mon"),
                                             predictors.op = "mean",
                                             special.predictors = list(list("pax", 18, "mean")),
                                             time.predictors.prior = 1:18,
                                             dependent = "pax",
                                             unit.variable = "air_number",
                                             unit.names.variable = "airport",
                                             time.variable = "time3",
                                             treated.units = 13:14,
                                             control.units = 1:12,
                                             time.optimize.ssr = 1:18,
                                             treatment.time = 19,
                                             time.plot = 1:30)

multiple.synth.out21.type3 <- multiple.synth(foo = type3,
                                             predictors = c( "prod_industr_agg", "tick_pr", "oil_import_pr_mon"),
                                             predictors.op = "mean",
                                             special.predictors = list(list("pax", 18, "mean")),
                                             time.predictors.prior = 1:18,
                                             dependent = "pax",
                                             unit.variable = "air_number",
                                             unit.names.variable = "airport",
                                             time.variable = "time3",
                                             treated.units = 26:27,
                                             control.units = 1:25,
                                             time.optimize.ssr = 1:18,
                                             treatment.time = 19,
                                             time.plot = 1:30)

multiple.synth.out21.type4 <- multiple.synth(foo = type4,
                                             predictors = c("prod_industr_agg", "tick_pr", "oil_import_pr_mon"),
                                             predictors.op = "mean",
                                             special.predictors = list(list("pax", 18, "mean")),
                                             time.predictors.prior = 1:18,
                                             dependent = "pax",
                                             unit.variable = "air_number",
                                             unit.names.variable = "airport",
                                             time.variable = "time3",
                                             treated.units = 25:32,
                                             control.units = 1:24,
                                             time.optimize.ssr = 1:18,
                                             treatment.time = 19,
                                             time.plot = 1:30)
#Extracting all the output from the multi-lists output
synth.outAMS11 <- synth(dataprepoutAMS11)

synth.outAMS1 <- multiple.synth.out1[["df"]][[1]][["synth.out"]]
dataprepoutAMS1 <- multiple.synth.out1[["df"]][[1]][["dataprep.out"]]
synth.outANR1 <- multiple.synth.out1[["df"]][[2]][["synth.out"]]
dataprepoutANR1 <- multiple.synth.out1[["df"]][[2]][["dataprep.out"]]
synth.outBRU1 <- multiple.synth.out1[["df"]][[3]][["synth.out"]]
dataprepoutBRU1 <- multiple.synth.out1[["df"]][[3]][["dataprep.out"]]
synth.outCRL1 <- multiple.synth.out1[["df"]][[4]][["synth.out"]]
dataprepoutCRL1 <- multiple.synth.out1[["df"]][[4]][["dataprep.out"]]
synth.outDUS1 <- multiple.synth.out1[["df"]][[5]][["synth.out"]]
dataprepoutDUS1 <- multiple.synth.out1[["df"]][[5]][["dataprep.out"]]
synth.outEIN1 <- multiple.synth.out1[["df"]][[6]][["synth.out"]]
dataprepoutEIN1 <- multiple.synth.out1[["df"]][[6]][["dataprep.out"]]
synth.outGRQ1 <- multiple.synth.out1[["df"]][[7]][["synth.out"]]
dataprepoutGRQ1 <- multiple.synth.out1[["df"]][[7]][["dataprep.out"]]
synth.outCGN1 <- multiple.synth.out1[["df"]][[8]][["synth.out"]]
dataprepoutCGN1 <- multiple.synth.out1[["df"]][[8]][["dataprep.out"]]
synth.outLGG1 <- multiple.synth.out1[["df"]][[9]][["synth.out"]]
dataprepoutLGG1 <- multiple.synth.out1[["df"]][[9]][["dataprep.out"]]
synth.outMST1 <- multiple.synth.out1[["df"]][[10]][["synth.out"]]
dataprepoutMST1 <- multiple.synth.out1[["df"]][[10]][["dataprep.out"]]
synth.outFMO1 <- multiple.synth.out1[["df"]][[11]][["synth.out"]]
dataprepoutFMO1 <- multiple.synth.out1[["df"]][[11]][["dataprep.out"]]
synth.outNRN1 <- multiple.synth.out1[["df"]][[12]][["synth.out"]]
dataprepoutNRN1 <- multiple.synth.out1[["df"]][[12]][["dataprep.out"]]
synth.outRTM1 <- multiple.synth.out1[["df"]][[13]][["synth.out"]]
dataprepoutRTM1 <- multiple.synth.out1[["df"]][[13]][["dataprep.out"]]

#graphs for the counterfactuals
path.plot(synth.res = synth.outAMS1, dataprep.res = dataprepoutAMS1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("AMS", "Synthetic AMS"),
          Legend.position = "topleft",
          Main = "Counterfactual AMS using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outANR1, dataprep.res = dataprepoutANR1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("ANR", "Synthetic ANR"),
          Legend.position = "topleft",
          Main = "Counterfactual ANR using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outBRU1, dataprep.res = dataprepoutBRU1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("BRU", "Synthetic BRU"),
          Legend.position = "topleft",
          Main = "Counterfactual BRU using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outCRL1, dataprep.res = dataprepoutCRL1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("CRL", "Synthetic CRL"),
          Legend.position = "topleft",
          Main = "Counterfactual CRL using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outDUS1, dataprep.res = dataprepoutDUS1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("DUS", "Synthetic DUS"),
          Legend.position = "topleft",
          Main = "Counterfactual DUS using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outEIN1, dataprep.res = dataprepoutEIN1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("EIN", "Synthetic EIN"),
          Legend.position = "topleft",
          Main = "Counterfactual EIN using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outGRQ1, dataprep.res = dataprepoutGRQ1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("GRQ", "Synthetic GRQ"),
          Legend.position = "topleft",
          Main = "Counterfactual GRQ using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outCGN1, dataprep.res = dataprepoutCGN1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("CGN", "Synthetic CGN"),
          Legend.position = "topleft",
          Main = "Counterfactual CGN using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outMST1, dataprep.res = dataprepoutMST1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("MST", "Synthetic MST"),
          Legend.position = "topleft",
          Main = "Counterfactual MST using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outFMO1, dataprep.res = dataprepoutFMO1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("FMO", "Synthetic FMO"),
          Legend.position = "topleft",
          Main = "Counterfactual FMO using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outNRN1, dataprep.res = dataprepoutNRN1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("NRN", "Synthetic NRN"),
          Legend.position = "topleft",
          Main = "Counterfactual NRN using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outRTM1, dataprep.res = dataprepoutRTM1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("RTM", "Synthetic RTM"),
          Legend.position = "topleft",
          Main = "Counterfactual RTM using full data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outLGG1, dataprep.res = dataprepoutLGG1,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("LGG", "Synthetic LGG"),
          Legend.position = "topleft",
          Main = "Counterfactual LGG using full data")
abline(v = 18, lty = 2)

       
#Same process for restricted data
synth.outANR11 <- multiple.synth.out11.type4[["df"]][[1]][["synth.out"]]
dataprepoutANR11 <- multiple.synth.out11.type4[["df"]][[1]][["dataprep.out"]]
synth.outBRU11 <- multiple.synth.out11.type2[["df"]][[1]][["synth.out"]]
dataprepoutBRU11 <- multiple.synth.out11.type2[["df"]][[1]][["dataprep.out"]]
synth.outCRL11 <- multiple.synth.out11.type3[["df"]][[1]][["synth.out"]]
dataprepoutCRL11 <- multiple.synth.out11.type3[["df"]][[1]][["dataprep.out"]]
synth.outDUS11 <- multiple.synth.out11.type2[["df"]][[2]][["synth.out"]]
dataprepoutDUS11 <- multiple.synth.out11.type2[["df"]][[2]][["dataprep.out"]]
synth.outEIN11 <- multiple.synth.out11.type4[["df"]][[2]][["synth.out"]]
dataprepoutEIN11 <- multiple.synth.out11.type4[["df"]][[2]][["dataprep.out"]]
synth.outGRQ11 <- multiple.synth.out11.type4[["df"]][[3]][["synth.out"]]
dataprepoutGRQ11 <- multiple.synth.out11.type4[["df"]][[3]][["dataprep.out"]]
synth.outCGN11 <- multiple.synth.out11.type3[["df"]][[2]][["synth.out"]]
dataprepoutCGN11 <- multiple.synth.out11.type3[["df"]][[2]][["dataprep.out"]]
synth.outLGG11 <- multiple.synth.out11.type4[["df"]][[4]][["synth.out"]]
dataprepoutLGG11 <- multiple.synth.out11.type4[["df"]][[4]][["dataprep.out"]]
synth.outMST11 <- multiple.synth.out11.type4[["df"]][[5]][["synth.out"]]
dataprepoutMST11 <- multiple.synth.out11.type4[["df"]][[5]][["dataprep.out"]]
synth.outFMO11 <- multiple.synth.out11.type4[["df"]][[6]][["synth.out"]]
dataprepoutFMO11 <- multiple.synth.out11.type4[["df"]][[6]][["dataprep.out"]]
synth.outNRN11 <- multiple.synth.out11.type4[["df"]][[7]][["synth.out"]]
dataprepoutNRN11 <- multiple.synth.out11.type4[["df"]][[7]][["dataprep.out"]]
synth.outRTM11 <- multiple.synth.out11.type4[["df"]][[8]][["synth.out"]]
dataprepoutRTM11 <- multiple.synth.out11.type4[["df"]][[8]][["dataprep.out"]]

path.plot(synth.res = synth.outAMS11, dataprep.res = dataprepoutAMS11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("AMS", "Synthetic AMS"),
          Legend.position = "topleft",
          Main = "Counterfactual AMS using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outANR11, dataprep.res = dataprepoutANR11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("ANR", "Synthetic ANR"),
          Legend.position = "topleft",
          Main = "Counterfactual ANR using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outBRU11, dataprep.res = dataprepoutBRU11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("BRU", "Synthetic BRU"),
          Legend.position = "topleft",
          Main = "Counterfactual BRU using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outCRL11, dataprep.res = dataprepoutCRL11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("CRL", "Synthetic CRL"),
          Legend.position = "topleft",
          Main = "Counterfactual CRL using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outDUS11, dataprep.res = dataprepoutDUS11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("DUS", "Synthetic DUS"),
          Legend.position = "topleft",
          Main = "Counterfactual DUS using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outEIN11, dataprep.res = dataprepoutEIN11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("EIN", "Synthetic EIN"),
          Legend.position = "topleft",
          Main = "Counterfactual EIN using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outGRQ11, dataprep.res = dataprepoutGRQ11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("GRQ", "Synthetic GRQ"),
          Legend.position = "topleft",
          Main = "Counterfactual GRQ using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outCGN11, dataprep.res = dataprepoutCGN11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("CGN", "Synthetic CGN"),
          Legend.position = "topleft",
          Main = "Counterfactual CGN using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outMST11, dataprep.res = dataprepoutMST11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("MST", "Synthetic MST"),
          Legend.position = "topleft",
          Main = "Counterfactual MST using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outFMO11, dataprep.res = dataprepoutFMO11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("FMO", "Synthetic FMO"),
          Legend.position = "topleft",
          Main = "Counterfactual FMO using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outNRN11, dataprep.res = dataprepoutNRN11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("NRN", "Synthetic NRN"),
          Legend.position = "topleft",
          Main = "Counterfactual NRN using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outRTM11, dataprep.res = dataprepoutRTM11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("RTM", "Synthetic RTM"),
          Legend.position = "topleft",
          Main = "Counterfactual RTM using restricted data")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outLGG11, dataprep.res = dataprepoutLGG11,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("LGG", "Synthetic LGG"),
          Legend.position = "topleft",
          Main = "Counterfactual LGG using restricted data")
abline(v = 18, lty = 2)

#Now specifciation 2

synth.outANR21 <- multiple.synth.out21.type4[["df"]][[1]][["synth.out"]]
dataprepoutANR21 <- multiple.synth.out21.type4[["df"]][[1]][["dataprep.out"]]
synth.outBRU21 <- multiple.synth.out21.type2[["df"]][[1]][["synth.out"]]
dataprepoutBRU21 <- multiple.synth.out21.type2[["df"]][[1]][["dataprep.out"]]
synth.outCRL21 <- multiple.synth.out21.type3[["df"]][[1]][["synth.out"]]
dataprepoutCRL21 <- multiple.synth.out21.type3[["df"]][[1]][["dataprep.out"]]
synth.outDUS21 <- multiple.synth.out21.type2[["df"]][[2]][["synth.out"]]
dataprepoutDUS21 <- multiple.synth.out21.type2[["df"]][[2]][["dataprep.out"]]
synth.outEIN21 <- multiple.synth.out21.type4[["df"]][[2]][["synth.out"]]
dataprepoutEIN21 <- multiple.synth.out21.type4[["df"]][[2]][["dataprep.out"]]
synth.outGRQ21 <- multiple.synth.out21.type4[["df"]][[3]][["synth.out"]]
dataprepoutGRQ21 <- multiple.synth.out21.type4[["df"]][[3]][["dataprep.out"]]
synth.outCGN21 <- multiple.synth.out21.type3[["df"]][[2]][["synth.out"]]
dataprepoutCGN21 <- multiple.synth.out21.type3[["df"]][[2]][["dataprep.out"]]
synth.outLGG21 <- multiple.synth.out21.type4[["df"]][[4]][["synth.out"]]
dataprepoutLGG21 <- multiple.synth.out21.type4[["df"]][[4]][["dataprep.out"]]
synth.outMST21 <- multiple.synth.out21.type4[["df"]][[5]][["synth.out"]]
dataprepoutMST21 <- multiple.synth.out21.type4[["df"]][[5]][["dataprep.out"]]
synth.outFMO21 <- multiple.synth.out21.type4[["df"]][[6]][["synth.out"]]
dataprepoutFMO21 <- multiple.synth.out21.type4[["df"]][[6]][["dataprep.out"]]
synth.outNRN21 <- multiple.synth.out21.type4[["df"]][[7]][["synth.out"]]
dataprepoutNRN21 <- multiple.synth.out21.type4[["df"]][[7]][["dataprep.out"]]
synth.outRTM21 <- multiple.synth.out21.type4[["df"]][[8]][["synth.out"]]
dataprepoutRTM21 <- multiple.synth.out21.type4[["df"]][[8]][["dataprep.out"]]


synth.outAMS2 <- multiple.synth.out2[["df"]][[1]][["synth.out"]]
dataprepoutAMS2 <- multiple.synth.out2[["df"]][[1]][["dataprep.out"]]
synth.outANR2 <- multiple.synth.out2[["df"]][[2]][["synth.out"]]
dataprepoutANR2 <- multiple.synth.out2[["df"]][[2]][["dataprep.out"]]
synth.outBRU2 <- multiple.synth.out2[["df"]][[3]][["synth.out"]]
dataprepoutBRU2 <- multiple.synth.out2[["df"]][[3]][["dataprep.out"]]
synth.outCRL2 <- multiple.synth.out2[["df"]][[4]][["synth.out"]]
dataprepoutCRL2 <- multiple.synth.out2[["df"]][[4]][["dataprep.out"]]
synth.outDUS2 <- multiple.synth.out2[["df"]][[5]][["synth.out"]]
dataprepoutDUS2 <- multiple.synth.out2[["df"]][[5]][["dataprep.out"]]
synth.outEIN2 <- multiple.synth.out2[["df"]][[6]][["synth.out"]]
dataprepoutEIN2 <- multiple.synth.out2[["df"]][[6]][["dataprep.out"]]
synth.outGRQ2 <- multiple.synth.out2[["df"]][[7]][["synth.out"]]
dataprepoutGRQ2 <- multiple.synth.out2[["df"]][[7]][["dataprep.out"]]
synth.outCGN2 <- multiple.synth.out2[["df"]][[8]][["synth.out"]]
dataprepoutCGN2 <- multiple.synth.out2[["df"]][[8]][["dataprep.out"]]
synth.outLGG2 <- multiple.synth.out2[["df"]][[9]][["synth.out"]]
dataprepoutLGG2 <- multiple.synth.out2[["df"]][[9]][["dataprep.out"]]
synth.outMST2 <- multiple.synth.out2[["df"]][[10]][["synth.out"]]
dataprepoutMST2 <- multiple.synth.out2[["df"]][[10]][["dataprep.out"]]
synth.outFMO2 <- multiple.synth.out2[["df"]][[11]][["synth.out"]]
dataprepoutFMO2 <- multiple.synth.out2[["df"]][[11]][["dataprep.out"]]
synth.outNRN2 <- multiple.synth.out2[["df"]][[12]][["synth.out"]]
dataprepoutNRN2 <- multiple.synth.out2[["df"]][[12]][["dataprep.out"]]
synth.outRTM2 <- multiple.synth.out2[["df"]][[13]][["synth.out"]]
dataprepoutRTM2 <- multiple.synth.out2[["df"]][[13]][["dataprep.out"]]

path.plot(synth.res = synth.outAMS21, dataprep.res = dataprepoutAMS21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("AMS", "Synthetic AMS"),
          Legend.position = "topleft",
          Main = "Counterfactual AMS using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outANR21, dataprep.res = dataprepoutANR21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("ANR", "Synthetic ANR"),
          Legend.position = "topleft",
          Main = "Counterfactual ANR using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outBRU21, dataprep.res = dataprepoutBRU21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("BRU", "Synthetic BRU"),
          Legend.position = "topleft",
          Main = "Counterfactual BRU using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outCRL21, dataprep.res = dataprepoutCRL21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("CRL", "Synthetic CRL"),
          Legend.position = "topleft",
          Main = "Counterfactual CRL using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outDUS21, dataprep.res = dataprepoutDUS21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("DUS", "Synthetic DUS"),
          Legend.position = "topleft",
          Main = "Counterfactual DUS using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outEIN21, dataprep.res = dataprepoutEIN21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("EIN", "Synthetic EIN"),
          Legend.position = "topleft",
          Main = "Counterfactual EIN using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outGRQ21, dataprep.res = dataprepoutGRQ21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("GRQ", "Synthetic GRQ"),
          Legend.position = "topleft",
          Main = "Counterfactual GRQ using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outCGN21, dataprep.res = dataprepoutCGN21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("CGN", "Synthetic CGN"),
          Legend.position = "topleft",
          Main = "Counterfactual CGN using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outMST21, dataprep.res = dataprepoutMST21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("MST", "Synthetic MST"),
          Legend.position = "topleft",
          Main = "Counterfactual MST using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outFMO21, dataprep.res = dataprepoutFMO21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("FMO", "Synthetic FMO"),
          Legend.position = "topleft",
          Main = "Counterfactual FMO using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outNRN21, dataprep.res = dataprepoutNRN21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("NRN", "Synthetic NRN"),
          Legend.position = "topleft",
          Main = "Counterfactual NRN using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outRTM21, dataprep.res = dataprepoutRTM21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("RTM", "Synthetic RTM"),
          Legend.position = "topleft",
          Main = "Counterfactual RTM using restricted data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outLGG21, dataprep.res = dataprepoutLGG21,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("LGG", "Synthetic LGG"),
          Legend.position = "topleft",
          Main = "Counterfactual LGG using restricted data \n (specification 2)")
abline(v = 18, lty = 2)




path.plot(synth.res = synth.outAMS2, dataprep.res = dataprepoutAMS2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("AMS", "Synthetic AMS"),
          Legend.position = "topleft",
          Main = "Counterfactual AMS using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outANR2, dataprep.res = dataprepoutANR2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("ANR", "Synthetic ANR"),
          Legend.position = "topleft",
          Main = "Counterfactual ANR using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outBRU2, dataprep.res = dataprepoutBRU2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("BRU", "Synthetic BRU"),
          Legend.position = "topleft",
          Main = "Counterfactual BRU using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outCRL2, dataprep.res = dataprepoutCRL2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("CRL", "Synthetic CRL"),
          Legend.position = "topleft",
          Main = "Counterfactual CRL using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outDUS2, dataprep.res = dataprepoutDUS2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("DUS", "Synthetic DUS"),
          Legend.position = "topleft",
          Main = "Counterfactual DUS using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outEIN2, dataprep.res = dataprepoutEIN2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("EIN", "Synthetic EIN"),
          Legend.position = "topleft",
          Main = "Counterfactual EIN using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outGRQ2, dataprep.res = dataprepoutGRQ2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("GRQ", "Synthetic GRQ"),
          Legend.position = "topleft",
          Main = "Counterfactual GRQ using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outCGN2, dataprep.res = dataprepoutCGN2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("CGN", "Synthetic CGN"),
          Legend.position = "topleft",
          Main = "Counterfactual CGN using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outMST2, dataprep.res = dataprepoutMST2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("MST", "Synthetic MST"),
          Legend.position = "topleft",
          Main = "Counterfactual MST using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outFMO2, dataprep.res = dataprepoutFMO2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("FMO", "Synthetic FMO"),
          Legend.position = "topleft",
          Main = "Counterfactual FMO using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outNRN2, dataprep.res = dataprepoutNRN2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("NRN", "Synthetic NRN"),
          Legend.position = "topleft",
          Main = "Counterfactual NRN using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outRTM2, dataprep.res = dataprepoutRTM2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("RTM", "Synthetic RTM"),
          Legend.position = "topleft",
          Main = "Counterfactual RTM using full data \n (specification 2)")
abline(v = 18, lty = 2)
path.plot(synth.res = synth.outLGG2, dataprep.res = dataprepoutLGG2,
          Ylab = "Number of passengers", Xlab = "time", 
          Legend = c("LGG", "Synthetic LGG"),
          Legend.position = "topleft",
          Main = "Counterfactual LGG using full data \n (specification 2)")
abline(v = 18, lty = 2)
#Extracting the W weights k
W_AMS1 <- round(synth.outAMS1$solution.w, 3)
W_AMS11 <- round(synth.outAMS11$solution.w, 3)
W_RTM1 <- round(synth.outRTM1$solution.w, 3)
W_RTM11 <- round(synth.outRTM11$solution.w, 3)
W_GRQ1 <- round(synth.outGRQ1$solution.w, 3)
W_GRQ11 <- round(synth.outGRQ11$solution.w, 3)
W_EIN1 <- round(synth.outEIN1$solution.w, 3)
W_EIN11 <- round(synth.outEIN11$solution.w, 3)
W_MST1 <- round(synth.outMST1$solution.w, 3)
W_MST11 <- round(synth.outMST11$solution.w, 3)

W_ANR1 <- round(synth.outANR1$solution.w, 3)
W_ANR11 <- round(synth.outANR11$solution.w, 3)
W_BRU1 <- round(synth.outBRU1$solution.w, 3)
W_BRU11 <- round(synth.outBRU11$solution.w, 3)
W_CGN1 <- round(synth.outCGN1$solution.w, 3)
W_CGN11 <- round(synth.outCGN11$solution.w, 3)
W_CRL1 <- round(synth.outCRL1$solution.w, 3)
W_CRL11 <- round(synth.outCRL11$solution.w, 3)
W_DUS1 <- round(synth.outDUS1$solution.w, 3)
W_DUS11 <- round(synth.outDUS11$solution.w, 3)
W_FMO1 <- round(synth.outFMO1$solution.w, 3)
W_FMO11 <- round(synth.outFMO11$solution.w, 3)
W_LGG1 <- round(synth.outLGG1$solution.w, 3)
W_LGG11 <- round(synth.outLGG11$solution.w, 3)
W_NRN1 <- round(synth.outNRN1$solution.w, 3)
W_NRN11 <- round(synth.outNRN11$solution.w, 3)

#Here I make the placebo runs. As you can see starting with NRN1 I use significance 
#ipop = 1 and multiprocess. This is because the computation time is faster, and the 
#change in results was negligible.

placeboAMS1 <- generate.placebos(dataprepoutAMS1, synth.outAMS1, Sigf.ipop = 2)
placeboRTM1 <- generate.placebos(dataprepoutRTM1, synth.outRTM1, Sigf.ipop = 2)
placeboEIN1 <- generate.placebos(dataprepoutEIN1, synth.outEIN1, Sigf.ipop = 2)
placeboGRQ1 <- generate.placebos(dataprepoutGRQ1, synth.outGRQ1, Sigf.ipop = 2)
placeboMST1 <- generate.placebos(dataprepoutMST1, synth.outMST1, Sigf.ipop = 2)
placeboDUS1 <- generate.placebos(dataprepoutDUS1, synth.outDUS1, Sigf.ipop = 2)
placeboNRN1 <- generate.placebos(dataprepoutNRN1, synth.outNRN1, Sigf.ipop = 1, strategy = "multiprocess")
placeboFMO1 <- generate.placebos(dataprepoutFMO1, synth.outFMO1, Sigf.ipop = 1, strategy = "multiprocess")
placeboCGN1 <- generate.placebos(dataprepoutCGN1, synth.outCGN1, Sigf.ipop = 1, strategy = "multiprocess")
placeboBRU1 <- generate.placebos(dataprepoutBRU1, synth.outBRU1, Sigf.ipop = 1, strategy = "multiprocess")
placeboCRL1 <- generate.placebos(dataprepoutCRL1, synth.outCRL1, Sigf.ipop = 1, strategy = "multiprocess")
placeboANR1 <- generate.placebos(dataprepoutANR1, synth.outANR1, Sigf.ipop = 1, strategy = "multiprocess")
placeboLGG1 <- generate.placebos(dataprepoutLGG1, synth.outLGG1, Sigf.ipop = 1, strategy = "multiprocess")

placeboAMS2 <- generate.placebos(dataprepoutAMS2, synth.outAMS2, Sigf.ipop = 1, strategy = "multiprocess")
placeboRTM2 <- generate.placebos(dataprepoutRTM2, synth.outRTM2, Sigf.ipop = 1, strategy = "multiprocess")
placeboEIN2 <- generate.placebos(dataprepoutEIN2, synth.outEIN2, Sigf.ipop = 1, strategy = "multiprocess")
placeboGRQ2 <- generate.placebos(dataprepoutGRQ2, synth.outGRQ2, Sigf.ipop = 1, strategy = "multiprocess")
placeboMST2 <- generate.placebos(dataprepoutMST2, synth.outMST2, Sigf.ipop = 1, strategy = "multiprocess")
placeboDUS2 <- generate.placebos(dataprepoutDUS2, synth.outDUS2, Sigf.ipop = 1, strategy = "multiprocess")
placeboNRN2 <- generate.placebos(dataprepoutNRN2, synth.outNRN2, Sigf.ipop = 1, strategy = "multiprocess")
placeboFMO2 <- generate.placebos(dataprepoutFMO2, synth.outFMO2, Sigf.ipop = 1, strategy = "multiprocess")
placeboCGN2 <- generate.placebos(dataprepoutCGN2, synth.outCGN2, Sigf.ipop = 1, strategy = "multiprocess")
placeboBRU2 <- generate.placebos(dataprepoutBRU2, synth.outBRU2, Sigf.ipop = 1, strategy = "multiprocess")
placeboCRL2 <- generate.placebos(dataprepoutCRL2, synth.outCRL2, Sigf.ipop = 1, strategy = "multiprocess")
placeboANR2 <- generate.placebos(dataprepoutANR2, synth.outANR2, Sigf.ipop = 1, strategy = "multiprocess")
placeboLGG2 <- generate.placebos(dataprepoutLGG2, synth.outLGG2, Sigf.ipop = 2)

#Plot the placebos and get MSPE ratio and p value

plot_placebos(tdf = placeboAMS1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeAMS1 <- mspe.test(placeboAMS1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboRTM1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeRTM1 <- mspe.test(placeboRTM1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboEIN1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeEIN1 <- mspe.test(placeboEIN1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboGRQ1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeGRQ1 <- mspe.test(placeboGRQ1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboMST1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeMST1 <- mspe.test(placeboMST1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboDUS1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeDUS1 <- mspe.test(placeboDUS1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboNRN1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeNRN1 <- mspe.test(placeboNRN1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboFMO1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeFMO1 <- mspe.test(placeboFMO1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboCGN1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeCGN1 <- mspe.test(placeboCGN1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboBRU1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeBRU1 <- mspe.test(placeboBRU1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboCRL1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeCRL1 <- mspe.test(placeboCRL1, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboANR1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeANR1 <- mspe.test(placeboANR1, discard.extreme = TRUE, mspe.limit = 8)
plot_placebos(tdf = placeboLGG1, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeLGG1 <- mspe.test(placeboLGG1)




plot_placebos(tdf = placeboAMS2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeAMS2 <- mspe.test(placeboAMS2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboRTM2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeRTM2 <- mspe.test(placeboRTM2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboEIN2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeEIN2 <- mspe.test(placeboEIN2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboGLQ2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeGRQ2 <- mspe.test(placeboGRQ2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboMST2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeMST2 <- mspe.test(placeboMST2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboDUS2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeDUS2 <- mspe.test(placeboDUS2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboNRN2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeNRN2 <- mspe.test(placeboNRN2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboFMO2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeFMO2 <- mspe.test(placeboFMO2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboCGN2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeCGN2 <- mspe.test(placeboCGN2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboBRU2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeBRU2 <- mspe.test(placeboBRU2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboCRL2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeCRL2 <- mspe.test(placeboCRL2, discard.extreme = TRUE, mspe.limit = 5)
plot_placebos(tdf = placeboANR2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeANR2 <- mspe.test(placeboANR2)
plot_placebos(tdf = placeboLGG2, ylab = "gap in the number of departing passengers",
              xlab = "time")
mspeLGG2 <- mspe.test(placeboLGG2)

#Distribtion of MSPE ratio plot I have in my thesis 
mspe.plot(placeboAMS1, plot.hist = TRUE)

#Extracting v weights
v_AMS1 <- round(synth.outAMS1$solution.v, 7)
v_AMS11 <- round(synth.outAMS11$solution.v, 7)
v_RTM1 <- round(synth.outRTM1$solution.v, 7)
v_RTM11 <- round(synth.outRTM11$solution.v, 7)
v_GRQ1 <- round(synth.outGRQ1$solution.v, 7)
v_GRQ11 <- round(synth.outGRQ11$solution.v, 7)
v_EIN1 <- round(synth.outEIN1$solution.v, 7)
v_EIN11 <- round(synth.outEIN11$solution.v, 7)
v_MST1 <- round(synth.outMST1$solution.v, 7)
v_MST11 <- round(synth.outMST11$solution.v, 7)

v_ANR1 <- round(synth.outANR1$solution.v, 7)
v_ANR11 <- round(synth.outANR11$solution.v, 7)
v_BRU1 <- round(synth.outBRU1$solution.v, 7)
v_BRU11 <- round(synth.outBRU11$solution.v, 7)
v_CGN1 <- round(synth.outCGN1$solution.v, 7)
v_CGN11 <- round(synth.outCGN11$solution.v, 7)
v_CRL1 <- round(synth.outCRL1$solution.v, 7)
v_CRL11 <- round(synth.outCRL11$solution.v, 7)
v_DUS1 <- round(synth.outDUS1$solution.v, 7)
v_DUS11 <- round(synth.outDUS11$solution.v, 7)
v_FMO1 <- round(synth.outFMO1$solution.v, 7)
v_FMO11 <- round(synth.outFMO11$solution.v, 7)
v_LGG1 <- round(synth.outLGG1$solution.v, 7)
v_LGG11 <- round(synth.outLGG11$solution.v, 7)
v_NRN1 <- round(synth.outNRN1$solution.v, 7)
v_NRN11 <- round(synth.outNRN11$solution.v, 7)




v_AMS2 <- round(synth.outAMS1$solution.v, 7)
v_AMS21 <- round(synth.outAMS11$solution.v, 7)
v_RTM2 <- round(synth.outRTM1$solution.v, 7)
v_RTM21 <- round(synth.outRTM11$solution.v, 7)
v_GRQ2 <- round(synth.outGRQ1$solution.v, 7)
v_GRQ21 <- round(synth.outGRQ11$solution.v, 7)
v_EIN2 <- round(synth.outEIN1$solution.v, 7)
v_EIN21 <- round(synth.outEIN11$solution.v, 7)
v_MST2 <- round(synth.outMST1$solution.v, 7)
v_MST21 <- round(synth.outMST11$solution.v, 7)

v_ANR2 <- round(synth.outANR1$solution.v, 7)
v_ANR21 <- round(synth.outANR11$solution.v, 7)
v_BRU2 <- round(synth.outBRU1$solution.v, 7)
v_BRU21 <- round(synth.outBRU11$solution.v, 7)
v_CGN2 <- round(synth.outCGN1$solution.v, 7)
v_CGN21 <- round(synth.outCGN11$solution.v, 7)
v_CRL2 <- round(synth.outCRL1$solution.v, 7)
v_CRL21 <- round(synth.outCRL11$solution.v, 7)
v_DUS2 <- round(synth.outDUS1$solution.v, 7)
v_DUS21 <- round(synth.outDUS11$solution.v, 7)
v_FMO2 <- round(synth.outFMO1$solution.v, 7)
v_FMO21 <- round(synth.outFMO11$solution.v, 7)
v_LGG2 <- round(synth.outLGG1$solution.v, 7)
v_LGG21 <- round(synth.outLGG11$solution.v, 7)
v_NRN2 <- round(synth.outNRN1$solution.v, 7)
v_NRN21 <- round(synth.outNRN11$solution.v, 7)

#I change the names so that they all match so that I can apply sumary statistics 
colnames(v_AMS1)[4] <- "oil_import_pr_mon"
colnames(v_AMS2)[4] <- "oil_import_pr_mon"
colnames(v_AMS11)[4] <- "oil_import_pr_mon"
colnames(v_AMS21)[4] <- "oil_import_pr_mon"
colnames(v_RTM1)[4] <- "oil_import_pr_mon"
colnames(v_RTM2)[4] <- "oil_import_pr_mon"
colnames(v_RTM11)[4] <- "oil_import_pr_mon"
colnames(v_RTM21)[4] <- "oil_import_pr_mon"
colnames(v_EIN1)[4] <- "oil_import_pr_mon"
colnames(v_EIN2)[4] <- "oil_import_pr_mon"
colnames(v_EIN11)[4] <- "oil_import_pr_mon"
colnames(v_EIN21)[4] <- "oil_import_pr_mon"
colnames(v_MST1)[4] <- "oil_import_pr_mon"
colnames(v_MST2)[4] <- "oil_import_pr_mon"
colnames(v_MST11)[4] <- "oil_import_pr_mon"
colnames(v_MST21)[4] <- "oil_import_pr_mon"
colnames(v_GRQ1)[4] <- "oil_import_pr_mon"
colnames(v_GRQ2)[4] <- "oil_import_pr_mon"
colnames(v_GRQ11)[4] <- "oil_import_pr_mon"
colnames(v_GRQ21)[4] <- "oil_import_pr_mon"
colnames(v_BRU1)[4] <- "oil_import_pr_mon"
colnames(v_BRU2)[4] <- "oil_import_pr_mon"
colnames(v_BRU11)[4] <- "oil_import_pr_mon"
colnames(v_BRU21)[4] <- "oil_import_pr_mon"
colnames(v_CRL1)[4] <- "oil_import_pr_mon"
colnames(v_CRL2)[4] <- "oil_import_pr_mon"
colnames(v_CRL11)[4] <- "oil_import_pr_mon"
colnames(v_CRL21)[4] <- "oil_import_pr_mon"
colnames(v_ANR1)[4] <- "oil_import_pr_mon"
colnames(v_ANR2)[4] <- "oil_import_pr_mon"
colnames(v_ANR11)[4] <- "oil_import_pr_mon"
colnames(v_ANR21)[4] <- "oil_import_pr_mon"
colnames(v_LGG1)[4] <- "oil_import_pr_mon"
colnames(v_LGG2)[4] <- "oil_import_pr_mon"
colnames(v_LGG11)[4] <- "oil_import_pr_mon"
colnames(v_LGG21)[4] <- "oil_import_pr_mon"
colnames(v_DUS1)[4] <- "oil_import_pr_mon"
colnames(v_DUS2)[4] <- "oil_import_pr_mon"
colnames(v_DUS11)[4] <- "oil_import_pr_mon"
colnames(v_DUS21)[4] <- "oil_import_pr_mon"
colnames(v_FMO1)[4] <- "oil_import_pr_mon"
colnames(v_FMO2)[4] <- "oil_import_pr_mon"
colnames(v_FMO11)[4] <- "oil_import_pr_mon"
colnames(v_FMO21)[4] <- "oil_import_pr_mon"
colnames(v_NRN1)[4] <- "oil_import_pr_mon"
colnames(v_NRN2)[4] <- "oil_import_pr_mon"
colnames(v_NRN11)[4] <- "oil_import_pr_mon"
colnames(v_NRN21)[4] <- "oil_import_pr_mon"
colnames(v_CGN1)[4] <- "oil_import_pr_mon"
colnames(v_CGN2)[4] <- "oil_import_pr_mon"
colnames(v_CGN11)[4] <- "oil_import_pr_mon"
colnames(v_CGN21)[4] <- "oil_import_pr_mon"


big_V <- rbind(v_AMS1, v_AMS11, v_AMS2, v_AMS21, v_RTM1, v_RTM11, v_RTM21, v_RTM2,
               v_EIN1, v_EIN11, v_EIN21, v_EIN2, v_GRQ1, v_GRQ11, v_GRQ21, v_GRQ2, 
               v_MST1, v_MST11, v_MST21, v_MST2, v_DUS1, v_DUS11, v_DUS21, v_DUS2,
               v_FMO1, v_FMO11, v_FMO21, v_FMO2, v_NRN1, v_NRN11, v_NRN21, v_NRN2, 
               v_CGN1, v_CGN11, v_CGN21, v_CGN2, v_BRU1, v_BRU11, v_BRU21, v_BRU2,
               v_CRL1, v_CRL11, v_CRL21, v_CRL2, v_ANR1, v_ANR11, v_ANR21, v_ANR2,
               v_LGG1, v_LGG11, v_LGG21, v_LGG2)

summary(big_V)
data <- select(airports, pax, tick_pr, prod_industr_agg, oil_import_pr_mon)
cor(data)






