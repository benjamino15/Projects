library(haven)
library(dplyr)
data_airports <- read_dta(file = "/Users/benjamin/R/data_thesis.dta")
data_airports2 <- filter(data_airports, time == "2007M01:2009M06" )
count_airports <- count(data_airports, airport, country)
air_number <- rep(c(1:345), each = 144)
data_airports <- cbind(data_airports, air_number)
data_airports <- select(data_airports, -air_number)
num_obs <- rep(seq(1:144), times = 345)
data_airports2 <- select(data_airports, air_number, airport, country, time, pax,
                         prod_industr_agg, tick_pr, oil_import_pr_mon)
numeric_dates <- readxl::read_excel("/Users/benjamin/R/numeric.dates.xlsx")
data_airports2 <- arrange(data_airports2, airport, time)
time2 <-as.vector(rep(numeric_dates$time3, times = 345))
data_airports2 <- cbind(data_airports2, time2)
data_airports2 <- select(data_airports2, air_number, airport, country, time, 
                         time2, pax, prod_industr_agg, tick_pr, oil_import_pr_mon)
class(data_airports2$time2)
data_airports2 <- filter(data_airports2, time2 %in% c(200701:200906))
count(count_airports$airport)
data_airports2 <- filter(data_airports2, !country == "United Kingdom" & 
                           !country == "Ireland" & !country == "Denmark" &
                           !country == "Malta")
filter(data_airports2, country == "Hungary")
missing_values <- readxl::read_excel("/Users/benjamin/R/missing_countries_oil_imports.xls")
library(tidyr)
replace(data_airports2$oil_import_pr_mon, ifelse(is.na(data_airports2$oil_import_pr_mon))  )
is.na(data_airports2$oil_import_pr_mon)
data_airports2$oil_import_pr_mon <- ifelse(data_airports2$country == 'Greece' & is.na(data_airports2$oil_import_pr_mon),
                                           missing_values$Greece, 
                                           data_airports2$oil_import_pr_mon) 
data_airports2$oil_import_pr_mon <- ifelse(data_airports2$country == 'Austria' & is.na(data_airports2$oil_import_pr_mon),
                                           missing_values$Austria, 
                                           data_airports2$oil_import_pr_mon) 
data_airports2$oil_import_pr_mon <- ifelse(data_airports2$country == 'Finland' & is.na(data_airports2$oil_import_pr_mon),
                                           missing_values$Finland, 
                                           data_airports2$oil_import_pr_mon) 
data_airports2$oil_import_pr_mon <- ifelse(data_airports2$country == 'Portugal' & is.na(data_airports2$oil_import_pr_mon),
                                           missing_values$Portugal, 
                                           data_airports2$oil_import_pr_mon) 
data_airports2$oil_import_pr_mon <- ifelse(data_airports2$country == 'Poland' & is.na(data_airports2$oil_import_pr_mon),
                                           missing_values$Poland, 
                                           data_airports2$oil_import_pr_mon)
data_airports2 <- filter(data_airports2, !country == "Croatia" & 
                           !country == "Cyprus" & !country == "Bulgaria" &
                           !country == "Estonia", !country == "Hungary", 
                           !country == "Latvia", !country == "Lithuania",
                           !country == "Luxembourg", !country == "Norway", 
                           !country == "Romania", !country == "Slovakia", 
                           !country == "Slovenia", !country == "Switzerland")
data_airports2 <- filter(data_airports2, !airport == "AIME CESAIRE/MARTINIQUE airport", 
                         !airport == "CAYENNE-FELIX-EBOUE airport",
                         !airport == "LA REUNION-ROLAND GARROS airport",
                         !airport == "POINTE-A-PITRE/LE RAIZET/GUADELOUPE airport",
                         !airport == "SAINT MARTIN, GRAND CASE, GUADELOUPE airport")
data_airports2 <- filter(data_airports2, !airport == "KALAMATA airport", 
                         !airport == "LAPPEENRANTA airport",
                         !airport == "MEMMINGEN airport",
                         !airport == "PANTELLERIA airport",
                         !airport == "PARDUBICE airport",
                         !airport == "SAVONLINNA airport",
                         !airport == "ZWEIBRUECKEN airport")
data_airports2 <- filter(data_airports2, !airport == "PERUGIA/S. FRANCESCO airport")
count_airports2 <- count(data_airports2, airport, country)
#Export final data
write.csv(data_airports2, file = "/Users/benjamin/R/final_data.csv")
summary(data_airports2$pax)
length(data_airports2$pax)
sd(data_airports2$pax)
summary(data_airports2$prod_industr_agg)
length(data_airports2$prod_industr_agg)
sd(data_airports2$prod_industr_agg)
summary(data_airports2$tick_pr)
length(data_airports2$tick_pr)
sd(data_airports2$tick_pr)
summary(data_airports2$oil_import_pr_mon)
length(data_airports2$oil_import_pr_mon)
sd(data_airports2$oil_import_pr_mon)
variable_df <- select(data_airports2, pax, prod_industr_agg, tick_pr, oil_import_pr_mon)
cov_matrix <-cor(variable_df, use = "all.obs")
hist(data_airports2$tick_pr, xlab = "Ticket Price \n (HICP flight ticket price, monthly, country)",
     main = "Histogram of Ticket Price")
hist(data_airports2$pax, xlab = "Departing passenger numbers \n (passengers carried, departures, monthly, airport)",
     main = "Histogram of Departing passengers")
log_passenger <- log(data_airports2$pax)
hist(log_passenger, xlab = "Log of departing passenger numbers \n (passengers carried, departures, monthly, airport)",
     main = "Histogram of Log of departing passengers")
hist(data_airports2$prod_industr_agg, xlab = "Industry output \n (Industrial production index, monthly, country)",
     main = "Histogram of Industry Output")
hist(data_airports2$oil_import_pr_mon, xlab = "Jet fuel price proxy \n (Oil import prices, monthly, country)",
     main = "Histogram Oil Import Prices")
shapiro.test(data_airports2$pax)
one_mil <- filter(data_airports2, pax > 1000000)
air_number <- rep(c(1:228), each = 30)
data_airports2$air_number <- air_number
data_airports2 <- read.csv(file = "/Users/benjamin/R/final_data.csv")
data_airports_var <- select(data_airports2, air_number, airport, time2, pax,
                            prod_industr_agg, tick_pr, oil_import_pr_mon)
data_airports_var <- filter(data_airports_var, time2 %in% c(200701:200806))
airports_var_treated <- filter(data_airports_var, airport == "AMSTERDAM/SCHIPHOL airport"| 
                               airport == "EINDHOVEN airport"| airport == "GRONINGEN/EELDE airport" |
                               airport == "MAASTRICHT/AACHEN airport" | airport == "ROTTERDAM airport" |
                               airport == "ANTWERPEN/DEURNE airport" | airport == "BRUSSELS airport" |
                               airport == "CHARLEROI/BRUSSELS SOUTH airport" | airport == "LIEGE airport"|
                               airport == "DUESSELDORF airport"| airport == "NIEDERRHEIN airport"|
                               airport == "MUENSTER/OSNABRUECK airport" | airport == "KOELN/BONN airport")
airports_var_untreated <-filter(data_airports_var, !airport == "AMSTERDAM/SCHIPHOL airport", 
                                !airport == "EINDHOVEN airport", !airport == "GRONINGEN/EELDE airport",
                                !airport == "MAASTRICHT/AACHEN airport", !airport == "ROTTERDAM airport",
                                !airport == "ANTWERPEN/DEURNE airport", !airport == "BRUSSELS airport",
                                !airport == "CHARLEROI/BRUSSELS SOUTH airport", !airport == "LIEGE airport",
                                !airport == "DUESSELDORF airport", !airport == "NIEDERRHEIN airport",
                                !airport == "MUENSTER/OSNABRUECK airport", !airport == "KOELN/BONN airport") 
airports_var_treated <- select(airports_var_treated, -airport, -time2)
airports_var_untreated <- select(airports_var_untreated, -time2)
airports_mean_untreated <- aggregate(x = airports_var_untreated, 
                                by = list(airports_var_untreated$air_number), 
                                FUN = mean)
apply(airports_mean_untreated, 2, min)
apply(airports_mean_untreated, 2, max)
airports_mean_treated <- aggregate(x = airports_var_treated, 
                                     by = list(airports_var_treated$air_number), 
                                     FUN = mean)
library(SCtools)
citation(package ="SCtools")
citation(package = "base") 
packageVersion("Synth")
packageVersion("SCtools")

airports_medium <- filter(airports, pax %in% 430000:700000)
airports_obs <- filter(airports, time3 == 1)
type1 <- filter(airports, airport == "PARIS-CHARLES DE GAULLE airport"| airport == "FRANKFURT/MAIN airport"| 
                        airport == "ADOLFO SUAREZ MADRID-BARAJAS airport" | airport == "BARCELONA/EL PRAT airport" |
                        airport == "MUENCHEN airport" | airport == "ROMA/FIUMICINO airport" |
                        airport == "AMSTERDAM/SCHIPHOL airport")
type2 <- filter(airports, airport == "WIEN-SCHWECHAT airport"| airport == "LISBOA airport"| 
                        airport == "ATHINAI/ELEFTHERIOS VENIZELOS airport" | airport == "PARIS-ORLY airport" |
                        airport == "MILANO/MALPENSA airport" | airport == "STOCKHOLM/ARLANDA airport" |
                        airport == "BERLIN-TEGEL airport" | airport == "WARSZAWA/CHOPINA airport" | 
                        airport == "HELSINKI-VANTAA airport" |airport == "PRAHA/RUZYNE airport"|
                        airport == "HAMBURG airport" | airport == "STUTTGART airport"|
                        airport == "BRUSSELS airport" | airport == "DUESSELDORF airport")
type3 <- filter(airports, airport == "PALMA DE MALLORCA airport" | airport == "MALAGA/COSTA DEL SOL airport"|
                        airport == "VALENCIA airport" | airport == "TOULOUSE/BLAGNAC airport" |
                        airport == "NANTES ATLANTIQUE airport" | airport == "BERLIN-SCHOENEFELD airport" |
                        airport == "FRANKFURT-HAHN airport" | airport == "NICE-COTE D AZUR airport" | 
                        airport == "BREMEN airport" |airport == "DORTMUND airport"|
                        airport == "LEIPZIG/HALLE airport" | airport == "LYON SAINT-EXUPERY airport"|
                        airport == "STOCKHOLM/SKAVSTA airport" | airport == "GOTEBORG/LANDVETTER airport" |
                        airport == "MALMO airport" | airport == "ROMA/CIAMPINO airport" |
                        airport == "MILANO/LINATE airport" | airport == "TORINO/CASELLE airport" |
                        airport == "KRAKOW/BALICE airport" | airport == "BORDEAUX-MERIGNAC airport" | 
                        airport == "HANNOVER airport" |airport == "NUERNBERG airport"|
                        airport == "GDANSK IM LECHA WALESY airport" | airport == "KATOWICE/PYRZOWICE airport"|
                        airport == "SALZBURG airport" | airport == "KOELN/BONN airport" |
                        airport == "CHARLEROI/BRUSSELS SOUTH airport")
type4 <- filter(airports, airport == "KARLOVY VARY airport" | airport == "KARLSTAD airport"|
                        airport == "SYLT airport" | airport == "LAAGE airport" |
                        airport == "OOSTENDE/BRUGGE airport" | airport == "STOCKHOLM/VASTERAS airport" |
                        airport == "METZ NANCY-LORRAINE airport" | airport == "ERFURT-WEIMAR airport" | 
                        airport == "LUEBECK-BLANKENSEE airport" |airport == "FRIEDRICHSHAFEN airport"|
                        airport == "LINZ airport" | airport == "GOTEBORG/SAVE airport"|
                        airport == "BALE-MULHOUSE airport" | airport == "GRAZ airport" |
                        airport == "INNSBRUCK airport" | airport == "LILLE-LESQUIN airport" |
                        airport == "KARLSRUHE/BADEN-BADEN airport" | airport == "PADERBORN/LIPPSTADT airport" |
                        airport == "WROCLAW/STRACHOWICE airport" | airport == "STRASBOURG-ENTZHEIM airport" | 
                        airport == "MALMO airport" |airport == "DRESDEN airport"|
                        airport == "STOCKHOLM/BROMMA airport" | airport == "BEAUVAIS-TILLE airport"|
                        airport == "EINDHOVEN airport" | airport == "ROTTERDAM airport" |
                        airport == "GRONINGEN/EELDE airport" |
                        airport == "MAASTRICHT/AACHEN airport" |airport == "NIEDERRHEIN airport"|
                        airport == "MUENSTER/OSNABRUECK airport" | airport == "ANTWERPEN/DEURNE airport"|
                        airport == "LIEGE airport")
air_number1 <- rep(c(1:7), each = 30)
air_number2 <- rep(c(1:14), each = 30)
air_number3 <- rep(c(1:27), each = 30)
air_number4 <- rep(c(1:32), each = 30)
type1$air_number <- replace(x = type1$air_number, values = air_number1)
type2$air_number <- replace(x = type2$air_number, values = air_number2)
type3$air_number <- replace(x = type3$air_number, values = air_number3)
type4$air_number <- replace(x = type4$air_number, values = air_number4)

library(readxl)
data_graph <- read_excel(path = "/Users/benjamin/R/Thesis/data_graph.xls", sheet = 2)
data_graph <- replace(data_graph, data_graph == "0", NA) 
names(data_graph)[1] <- "time"
library(zoo)
data_graph$time <- as.yearmon(data_graph$time)
library(ggplot2) 
library(ggsci)
library(gridExtra)

pal_npg()

#a6cee3
#1f78b4
#b2df8a
#33a02c

#fb9a99
#e31a1c
#fdbf6f
#ff7f00

#cab2d6
#6a3d9a
#ffff99
#b15928


colors <- c( "Rotterdam" = "#1f78b4", "Eindhoven" = "#b2df8a", 
             "Maastricht" = "#33a02c", "Groningen" = "#fb9a99", 
             "Antwerpen" = "#fdbf6f", "Charleroi" = "#ff7f00", "Liege" = "#cab2d6", 
             "Münster" = "#6a3d9a",  "Weeze" = "#b15928")
colors1 <- c("Amsterdam" = "#a6cee3") 
colors2 <- c("Düsseldorf" = "#ffff99", "Brussels" = "#e31a1c", "Cologne" = "#857161" )

p3 <- ggplot(data_graph, aes(x = time)) +
        geom_line(aes(y = Rotterdam, color = "Rotterdam")) + geom_line(aes(y =Eindhoven, color = "Eindhoven")) +
        geom_line(aes(y =Maastricht, color = "Maastricht")) + geom_line(aes(y =Groningen, color = "Groningen")) +
        geom_line(aes(y =Antwerpen, color = "Antwerpen")) + geom_line(aes(y = Charleroi, color = "Charleroi")) + 
        geom_line(aes(y = Liege, color = "Liege")) + geom_line(aes(y = Münster, color = "Münster")) +
        geom_line(aes(y = Weeze, color = "Weeze")) + 
        labs(x = "time", y = "number of departing \n passengers (in 000s)", color = "Legend") +
        scale_color_manual(values = colors) +  theme_bw()  + 
        theme(text = element_text(size=7)) + theme(aspect.ratio = 1/1.85) +
        annotate("rect", xmin = as.yearmon("2007-01"), xmax = as.yearmon("2009-06"), ymin = -Inf, ymax = Inf, alpha = .25) 
        

p1 <- ggplot(data_graph, aes(x = time)) + geom_line(aes(y = Amsterdam, color = "Amsterdam")) +
        labs(x = "time", y = "number of departing \n passengers (in 000s)", color = "Legend") +
        scale_color_manual(values = colors1) + theme_bw()  + scale_y_continuous(limit = c(1350, 2650)) +
        theme(text = element_text(size=7)) + theme(aspect.ratio = 1/1.85) +    
        annotate("rect", xmin = as.yearmon("2007-01"), xmax = as.yearmon("2009-06"), ymin = -Inf, ymax = Inf, alpha = .25)



p2 <- ggplot(data_graph, aes(x = time)) + geom_line(aes(y = Brussels, color = "Brussels")) +
        geom_line(aes(y = Düsseldorf, color = "Düsseldorf")) + geom_line(aes(y = Cologne, color = "Cologne")) +
        labs(x = "time", y = "number of departing \n passengers (in 000s)", color = "Legend") +
        scale_color_manual(values = colors2) + theme_bw() + scale_y_continuous(limit = c(275, 1060)) +
        theme(text = element_text(size=7)) + theme(aspect.ratio = 1/1.85) +    
        annotate("rect", xmin = as.yearmon("2007-01"), xmax = as.yearmon("2009-06"), ymin = -Inf, ymax = Inf, alpha = .25)

min(data_graph$Charleroi)
max(data_graph$Düsseldorf)
max(data_graph$Cologne)

 grid.arrange(p1, p2, p3, ncol = 1)
    

        
ggplot(data_graph, aes(x=time)) + geom_line(aes(y = Maastricht, color = "black")) + theme(aspect.ratio = 1/2)
 
 
 
 colors4 <- c( "Rotterdam" = "#1f78b4", "Eindhoven" = "#b2df8a", 
              "Maastricht" = "#33a02c", "Groningen" = "#fb9a99", 
              "Antwerpen" = "#fdbf6f", "Liege" = "#cab2d6", 
              "Münster" = "#6a3d9a",  "Weeze" = "#b15928")
 colors1 <- c("Amsterdam" = "#a6cee3") 
 colors2 <- c("Düsseldorf" = "#ffff99", "Brussels" = "#e31a1c")
 colors3 <- c("Cologne" = "#857161", "Charleroi" = "#ff7f00")
 
 p4 <- ggplot(data_graph, aes(x = time)) +
         geom_line(aes(y = Rotterdam, color = "Rotterdam")) + geom_line(aes(y =Eindhoven, color = "Eindhoven")) +
         geom_line(aes(y =Maastricht, color = "Maastricht")) + geom_line(aes(y =Groningen, color = "Groningen")) +
         geom_line(aes(y =Antwerpen, color = "Antwerpen")) +  geom_line(aes(y = Liege, color = "Liege")) + 
         geom_line(aes(y = Münster, color = "Münster")) + geom_line(aes(y = Weeze, color = "Weeze")) + 
         labs(x = "time", y = "number of departing \n passengers (in 000s)",
              title = "Regional/local airports", color = "Legend") +
         scale_color_manual(values = colors4) +  theme_bw() + 
         theme(legend.position = "bottom", legend.background = element_rect(fill="#F5F5F5")) +
         theme(text = element_text(size=7)) + theme(aspect.ratio = 1/2) +
         annotate("rect", xmin = as.yearmon("2007-01"), 
                  xmax = as.yearmon("2009-06"), ymin = -Inf, ymax = Inf, alpha = .25) +
         theme(legend.key.height = unit(0.3, "cm"))
 
 
 p1 <- ggplot(data_graph, aes(x = time)) + geom_line(aes(y = Amsterdam, color = "Amsterdam")) +
         labs(x = "time", y = "number of departing \n passengers (in 000s)", 
              title = "World-wide hub airport",  color = "Legend") +
         scale_color_manual(values = colors1) + theme_bw()  + 
         scale_y_continuous(limit = c(1350, 2650)) + theme(text = element_text(size=7)) +
         theme(aspect.ratio = 1/2) + 
         theme(legend.position = "bottom", legend.background = element_rect(fill="#F5F5F5")) + 
         annotate("rect", xmin = as.yearmon("2007-01"), xmax = as.yearmon("2009-06"), 
                  ymin = -Inf, ymax = Inf, alpha = .25)
 
 
 
 p2 <- ggplot(data_graph, aes(x = time)) + geom_line(aes(y = Brussels, color = "Brussels")) +
         geom_line(aes(y = Düsseldorf, color = "Düsseldorf"))  +
         labs(x = "time", y = "number of departing \n passengers (in 000s)", 
              title = "Hub airports", color = "Legend") +
         scale_color_manual(values = colors2) + theme_bw() + 
         scale_y_continuous(limit = c(420, 1060)) + theme(text = element_text(size=7)) + 
         theme(aspect.ratio = 1/2) + 
         theme(legend.position = "bottom", legend.background = element_rect(fill="#F5F5F5")) +
         annotate("rect", xmin = as.yearmon("2007-01"), xmax = as.yearmon("2009-06"),
                  ymin = -Inf, ymax = Inf, alpha = .25)
 
 p3 <- ggplot(data_graph, aes(x = time)) + geom_line(aes(y = Cologne, color = "Cologne")) +
         geom_line(aes(y = Charleroi, color = "Charleroi")) +
         labs(x = "time", y = "number of departing \n passengers (in 000s)", 
              title = "Medium (low-cost) airports" ,color = "Legend") +
         scale_color_manual(values = colors3) + theme_bw()  + 
         scale_y_continuous(limit = c(60, 565)) +
         theme(text = element_text(size=7)) + theme(aspect.ratio = 1/2) + 
         theme(legend.position = "bottom", legend.background = element_rect(fill="#F5F5F5")) + 
         annotate("rect", xmin = as.yearmon("2007-01"), xmax = as.yearmon("2009-06"),
                  ymin = -Inf, ymax = Inf, alpha = .25)

 
 grid.arrange(p1, p3, ncol = 1, nrow = 2)
 grid.arrange(p2, p4, ncol = 1, nrow = 2)
 



        