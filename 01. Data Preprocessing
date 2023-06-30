#Thesis Data building process
install.packages("dgof") #Normality Test
install.packages("rpart.plot") #Regression Tree Plot
install.packages("caTools") 
install.packages("ggpubr") #Correlation 
install.packages("MASS") #fitdstr 

#Library
library(readxl)
library(ggplot2)
library(writexl)
library(readxl)
library(dplyr)
library(openxlsx) #To export tabel payoff dari list ke format excel

#Input data
#------
setwd("/Users/victoriajanice/Documents/U N I/Thesis/Dataset")
Dataset <- read.csv("SDBS_BDI_ISIC4_18052023042159615-18 May 2023.csv")
Data_death <- Dataset[(Dataset$Variable=="Death rate of all enterprises"),]
Data_death <- Data_death[,c("Country","ISIC4","TIME","Value")]
colnames(Data_death) <- c("Country","Sector", "Year", "DR")

Data_birth <- Dataset[(Dataset$Variable=="Birth rate of all enterprises"),]
Data_birth <- Data_birth[,c("Country","ISIC4","TIME","Value")]
colnames(Data_birth) <- c("Country","Sector", "Year", "BR")

Data_num <-Dataset[(Dataset$Variable=="Number of active enterprises (all enterprises)"),]
Data_num <- Data_num[,c("Country","ISIC4","TIME","Value")]
colnames(Data_num) <- c("Country","Sector", "Year", "Pop") #Population = Number of active enterprises

Data_numdeaths <- Dataset[(Dataset$Variable=="Number of deaths (all enterprises)"),]
Data_numdeaths <- Data_numdeaths[,c("Country","ISIC4","TIME","Value")]
colnames(Data_numdeaths) <- c("Country","Sector", "Year", "ND") #Number of deaths

Data_Prod <- read.csv("SSIS_BSC_ISIC4_18052023043707673- 18 May 2023.csv")
Data_Prod <- Data_Prod[,c("Country","ISIC4.1","SRC","TIME","Unit.Code","Unit","Value")]
Data_Prod <- Data_Prod[!(Data_Prod$SRC == "BSC"),] #Drop source BSC use source SSIS (Structural Statistics of Industry & Services)
colnames(Data_Prod) <- c("Country","Sector","Source", "Year","Unit.Code","Unit","Value") #Productivity
#------

#Input new data macroeconomy factors
#------
mf_austria <- read_xls("SPGlobal_Austria_EconomicAndDemographicData_26-May-2023.xls")
mf_austria <- mf_austria[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_austria) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_belgium <- read_xls("SPGlobal_Belgium_EconomicAndDemographicData_26-May-2023.xls")
mf_belgium <- mf_belgium[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_belgium) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_bulgaria <- read_xls("SPGlobal_Bulgaria_EconomicAndDemographicData_26-May-2023.xls")
mf_bulgaria <- mf_bulgaria[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_bulgaria) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_croatia <- read_xls("SPGlobal_Croatia_EconomicAndDemographicData_26-May-2023.xls")
mf_croatia <- mf_croatia[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_croatia) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_cyprus <- read_xls("SPGlobal_Cyprus_EconomicAndDemographicData_26-May-2023.xls")
mf_cyprus <- mf_cyprus[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_cyprus) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_czechrepublic <- read_xls("SPGlobal_Czechia_EconomicAndDemographicData_26-May-2023.xls")
mf_czechrepublic <- mf_czechrepublic[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_czechrepublic) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_denmark <- read_xls("SPGlobal_Denmark_EconomicAndDemographicData_26-May-2023.xls")
mf_denmark <- mf_denmark[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_denmark) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_estonia <- read_xls("SPGlobal_Estonia_EconomicAndDemographicData_26-May-2023.xls")
mf_estonia <- mf_estonia[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_estonia) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_finland <- read_xls("SPGlobal_Finland_EconomicAndDemographicData_26-May-2023.xls")
mf_finland <- mf_finland[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_finland) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_france <- read_xls("SPGlobal_France_EconomicAndDemographicData_26-May-2023.xls")
mf_france <- mf_france[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_france) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_germany <- read_xls("SPGlobal_Germany_EconomicAndDemographicData_26-May-2023.xls")
mf_germany <- mf_germany[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_germany) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_greece <- read_xls("SPGlobal_Greece_EconomicAndDemographicData_26-May-2023.xls")
mf_greece <- mf_greece[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_greece) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_hungary <- read_xls("SPGlobal_Hungary_EconomicAndDemographicData_26-May-2023.xls")
mf_hungary <- mf_hungary[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_hungary) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_iceland <- read_xls("SPGlobal_Iceland_EconomicAndDemographicData_26-May-2023.xls")
mf_iceland <- mf_iceland[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_iceland) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_ireland <- read_xls("SPGlobal_Ireland_EconomicAndDemographicData_26-May-2023.xls")
mf_ireland <- mf_ireland[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_ireland) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_italy <- read_xls("SPGlobal_Italy_EconomicAndDemographicData_26-May-2023.xls")
mf_italy <- mf_italy[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_italy) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_latvia <- read_xls("SPGlobal_Latvia_EconomicAndDemographicData_26-May-2023.xls")
mf_latvia <- mf_latvia[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_latvia) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_lithuania <- read_xls("SPGlobal_Lithuania_EconomicAndDemographicData_26-May-2023.xls")
mf_lithuania <- mf_lithuania[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_lithuania) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_luxembourg <- read_xls("SPGlobal_Luxembourg_EconomicAndDemographicData_26-May-2023.xls")
mf_luxembourg <- mf_luxembourg[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_luxembourg) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_malta <- read_xls("SPGlobal_Malta_EconomicAndDemographicData_26-May-2023.xls")
mf_malta <- mf_malta[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_malta) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_netherlands <- read_xls("SPGlobal_Netherlands_EconomicAndDemographicData_26-May-2023.xls")
mf_netherlands <- mf_netherlands[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_netherlands) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_norway <- read_xls("SPGlobal_Norway_EconomicAndDemographicData_26-May-2023.xls")
mf_norway <- mf_norway[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_norway) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_poland <- read_xls("SPGlobal_Poland_EconomicAndDemographicData_26-May-2023.xls")
mf_poland <- mf_poland[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_poland) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_portugal <- read_xls("SPGlobal_Portugal_EconomicAndDemographicData_26-May-2023.xls")
mf_portugal <- mf_portugal[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_portugal) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_romania <- read_xls("SPGlobal_Romania_EconomicAndDemographicData_26-May-2023.xls")
mf_romania <- mf_romania[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_romania) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_serbia <- read_xls("SPGlobal_Serbia_EconomicAndDemographicData_26-May-2023.xls")
mf_serbia <- mf_serbia[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_serbia) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_slovakrepublic <- read_xls("SPGlobal_Slovakia_EconomicAndDemographicData_26-May-2023.xls")
mf_slovakrepublic <- mf_slovakrepublic[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_slovakrepublic) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_slovenia <- read_xls("SPGlobal_Slovenia_EconomicAndDemographicData_26-May-2023.xls")
mf_slovenia <- mf_slovenia[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_slovenia) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_spain <- read_xls("SPGlobal_Spain_EconomicAndDemographicData_26-May-2023.xls")
mf_spain <- mf_spain[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_spain) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_sweden <- read_xls("SPGlobal_Sweden_EconomicAndDemographicData_26-May-2023.xls")
mf_sweden <- mf_sweden[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_sweden) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_switzerland <- read_xls("SPGlobal_Switzerland_EconomicAndDemographicData_26-May-2023.xls")
mf_switzerland <- mf_switzerland[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_switzerland) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

mf_uk <- read_xls("SPGlobal_UnitedKingdom_EconomicAndDemographicData_26-May-2023.xls")
mf_uk <- mf_uk[-c(1:14,16:22,24:28,30:32,34:47,49:69), ]
colnames(mf_uk) <- c("Macroeconomy Factors","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")
#------

#check duplicate & remove na
#------
#summary(Data_death)
sum(duplicated(Data_death))
Data_death <- na.omit(Data_death)

#summary(Data_birth)
sum(duplicated(Data_birth))
Data_birth <-na.omit(Data_birth)

#summary(Data_num)
sum(duplicated(Data_num))
Data_num <- na.omit(Data_num)

#summary(Data_numdeaths)
sum(duplicated(Data_numdeaths))
Data_numdeaths <- na.omit(Data_numdeaths)

#summary(Data_Prod)
sum(duplicated(Data_Prod))
#str(Data_Prod)
Data_Prod <- na.omit(Data_Prod)
#------

#Remove unused sectors and country
#------
Data_d <- Data_death[!(Data_death$Sector=="Financial and insurance activities" | Data_death$Sector=="Education" | Data_death$Country == "Malta"),]
Data_d <- droplevels(Data_d)
Data_b <-Data_birth[!(Data_birth$Sector=="Financial and insurance activities" | Data_birth$Sector=="Education" | Data_birth$Country == "Malta"),]
Data_b <- droplevels(Data_b)
Data_n <-Data_num[!(Data_num$Sector=="Financial and insurance activities" | Data_num$Sector=="Education" | Data_num$Country == "Malta" ),]
Data_n <- droplevels(Data_n)
Data_nd <-Data_numdeaths[!(Data_numdeaths$Sector=="Financial and insurance activities" | Data_numdeaths$Sector=="Education" | Data_numdeaths$Country == "Malta"),]
Data_nd <- droplevels(Data_nd)
Data_p <- Data_Prod[!(Data_Prod$Sector=="Financial and insurance activities" | Data_Prod$Sector=="Education" | Data_Prod$Country == "Malta"),]
Data_p <- droplevels(Data_p)
#------

#Make All productivity data into Euro
#------
#Masukin RSD ke Dataframe
levels(Data_p$Unit.Code) <- c(levels(Data_p$Unit.Code),"RSD")
Data_p$Unit.Code[Data_p$Country == "Serbia"] <- "RSD"
Data_p$Unit.Code <- droplevels(Data_p$Unit.Code)
summary(Data_p$Unit.Code)

#Input Currency Exchange Rate
er <- read_xlsx("Currency Exchange Rate.xlsx", sheet = "Sheet3")
Data_p$Multiplier <- 1
Data_p$Multiplier[Data_p$Unit.Code == "BGN"] <- as.numeric(er[1,2])
Data_p$Multiplier[Data_p$Unit.Code == "CHF"] <- as.numeric(er[2,2])
Data_p$Multiplier[Data_p$Unit.Code == "CYP"] <- as.numeric(er[3,2])
Data_p$Multiplier[Data_p$Unit.Code == "CZK"] <- as.numeric(er[4,2])
Data_p$Multiplier[Data_p$Unit.Code == "DKK"] <- as.numeric(er[5,2])
Data_p$Multiplier[Data_p$Unit.Code == "GBP"] <- as.numeric(er[6,2])
Data_p$Multiplier[Data_p$Unit.Code == "HRK"] <- as.numeric(er[7,2])
Data_p$Multiplier[Data_p$Unit.Code == "HUF"] <- as.numeric(er[8,2])
Data_p$Multiplier[Data_p$Unit.Code == "ISK"] <- as.numeric(er[9,2])
Data_p$Multiplier[Data_p$Unit.Code == "NOK"] <- as.numeric(er[10,2])
Data_p$Multiplier[Data_p$Unit.Code == "PLN"] <- as.numeric(er[11,2])
Data_p$Multiplier[Data_p$Unit.Code == "RON"] <- as.numeric(er[12,2])
Data_p$Multiplier[Data_p$Unit.Code == "SEK"] <- as.numeric(er[13,2])
Data_p$Multiplier[Data_p$Unit.Code == "RSD"] <- as.numeric(er[14,2])

#Generating Euro Productivity
Data_p$Prod <- as.numeric(Data_p$Value) * as.numeric(Data_p$Multiplier)
Data_p <- Data_p[,c("Country","Sector","Year","Prod")]
#------

#Gabungin Data
#------
df <- merge(Data_d,Data_b, by.x = c("Country","Sector","Year"), all.x = FALSE)
df2 <- merge(merge(df,Data_n, by.x = c("Country","Sector","Year"), all.x = FALSE),Data_nd, by.x = c("Country","Sector","Year"), all.x = FALSE) #Buat Poisson Regression
fulldataset <- merge(df2,Data_p, by.x = c("Country","Sector","Year"), all.x = FALSE)
fulldataset$DR <- fulldataset$DR/100
fulldataset$BR <- fulldataset$BR/100
fulldataset$GDP <- 1 #GDP Per Capita ($)
fulldataset$UR <- 1 #Unemployment Rate
fulldataset$CPG <- 1 #Consumer Prices Inflation (%)
fulldataset$IR <- 1 #Money Market Interest Rate
fulldataset$D <- 1 #Debt/ GDP (%)
#------

#Masukin faktor makroekonomi data
#------
#Austria
for (i in 1:17){
fulldataset$GDP[fulldataset$Country == "Austria" & fulldataset$Year == 2003+i] <- as.numeric(mf_austria[1,1+i])
fulldataset$UR[fulldataset$Country == "Austria" & fulldataset$Year == 2003+i] <- as.numeric(mf_austria[5,1+i])/100
fulldataset$CPG[fulldataset$Country == "Austria" & fulldataset$Year == 2003+i] <- as.numeric(mf_austria[2,1+i])/100
fulldataset$IR[fulldataset$Country == "Austria" & fulldataset$Year == 2003+i] <- as.numeric(mf_austria[3,1+i])/100
fulldataset$D[fulldataset$Country == "Austria" & fulldataset$Year == 2003+i] <- as.numeric(mf_austria[4,1+i])/100
}

#Belgium
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Belgium" & fulldataset$Year == 2003+i] <- as.numeric(mf_belgium[1,1+i])
  fulldataset$UR[fulldataset$Country == "Belgium" & fulldataset$Year == 2003+i] <- as.numeric(mf_belgium[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Belgium" & fulldataset$Year == 2003+i] <- as.numeric(mf_belgium[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Belgium" & fulldataset$Year == 2003+i] <- as.numeric(mf_belgium[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Belgium" & fulldataset$Year == 2003+i] <- as.numeric(mf_belgium[4,1+i])/100
}

#Bulgaria
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Bulgaria" & fulldataset$Year == 2003+i] <- as.numeric(mf_bulgaria[1,1+i])
  fulldataset$UR[fulldataset$Country == "Bulgaria" & fulldataset$Year == 2003+i] <- as.numeric(mf_bulgaria[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Bulgaria" & fulldataset$Year == 2003+i] <- as.numeric(mf_bulgaria[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Bulgaria" & fulldataset$Year == 2003+i] <- as.numeric(mf_bulgaria[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Bulgaria" & fulldataset$Year == 2003+i] <- as.numeric(mf_bulgaria[4,1+i])/100
}

#Croatia
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Croatia" & fulldataset$Year == 2003+i] <- as.numeric(mf_croatia[1,1+i])
  fulldataset$UR[fulldataset$Country == "Croatia" & fulldataset$Year == 2003+i] <- as.numeric(mf_croatia[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Croatia" & fulldataset$Year == 2003+i] <- as.numeric(mf_croatia[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Croatia" & fulldataset$Year == 2003+i] <- as.numeric(mf_croatia[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Croatia" & fulldataset$Year == 2003+i] <- as.numeric(mf_croatia[4,1+i])/100
}

#Cyprus
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Cyprus" & fulldataset$Year == 2003+i] <- as.numeric(mf_cyprus[1,1+i])
  fulldataset$UR[fulldataset$Country == "Cyprus" & fulldataset$Year == 2003+i] <- as.numeric(mf_cyprus[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Cyprus" & fulldataset$Year == 2003+i] <- as.numeric(mf_cyprus[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Cyprus" & fulldataset$Year == 2003+i] <- as.numeric(mf_cyprus[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Cyprus" & fulldataset$Year == 2003+i] <- as.numeric(mf_cyprus[4,1+i])/100
}

#Czech Republic
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Czech Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_czechrepublic[1,1+i])
  fulldataset$UR[fulldataset$Country == "Czech Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_czechrepublic[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Czech Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_czechrepublic[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Czech Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_czechrepublic[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Czech Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_czechrepublic[4,1+i])/100
}

#Denmark
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Denmark" & fulldataset$Year == 2003+i] <- as.numeric(mf_denmark[1,1+i])
  fulldataset$UR[fulldataset$Country == "Denmark" & fulldataset$Year == 2003+i] <- as.numeric(mf_denmark[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Denmark" & fulldataset$Year == 2003+i] <- as.numeric(mf_denmark[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Denmark" & fulldataset$Year == 2003+i] <- as.numeric(mf_denmark[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Denmark" & fulldataset$Year == 2003+i] <- as.numeric(mf_denmark[4,1+i])/100
}

#Estonia
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Estonia" & fulldataset$Year == 2003+i] <- as.numeric(mf_estonia[1,1+i])
  fulldataset$UR[fulldataset$Country == "Estonia" & fulldataset$Year == 2003+i] <- as.numeric(mf_estonia[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Estonia" & fulldataset$Year == 2003+i] <- as.numeric(mf_estonia[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Estonia" & fulldataset$Year == 2003+i] <- as.numeric(mf_estonia[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Estonia" & fulldataset$Year == 2003+i] <- as.numeric(mf_estonia[4,1+i])/100
}

#Finland
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Finland" & fulldataset$Year == 2003+i] <- as.numeric(mf_finland[1,1+i])
  fulldataset$UR[fulldataset$Country == "Finland" & fulldataset$Year == 2003+i] <- as.numeric(mf_finland[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Finland" & fulldataset$Year == 2003+i] <- as.numeric(mf_finland[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Finland" & fulldataset$Year == 2003+i] <- as.numeric(mf_finland[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Finland" & fulldataset$Year == 2003+i] <- as.numeric(mf_finland[4,1+i])/100
}

#France
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "France" & fulldataset$Year == 2003+i] <- as.numeric(mf_france[1,1+i])
  fulldataset$UR[fulldataset$Country == "France" & fulldataset$Year == 2003+i] <- as.numeric(mf_france[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "France" & fulldataset$Year == 2003+i] <- as.numeric(mf_france[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "France" & fulldataset$Year == 2003+i] <- as.numeric(mf_france[3,1+i])/100
  fulldataset$D[fulldataset$Country == "France" & fulldataset$Year == 2003+i] <- as.numeric(mf_france[4,1+i])/100
}

#Germany
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Germany" & fulldataset$Year == 2003+i] <- as.numeric(mf_germany[1,1+i])
  fulldataset$UR[fulldataset$Country == "Germany" & fulldataset$Year == 2003+i] <- as.numeric(mf_germany[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Germany" & fulldataset$Year == 2003+i] <- as.numeric(mf_germany[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Germany" & fulldataset$Year == 2003+i] <- as.numeric(mf_germany[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Germany" & fulldataset$Year == 2003+i] <- as.numeric(mf_germany[4,1+i])/100
}

#Greece
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Greece" & fulldataset$Year == 2003+i] <- as.numeric(mf_greece[1,1+i])
  fulldataset$UR[fulldataset$Country == "Greece" & fulldataset$Year == 2003+i] <- as.numeric(mf_greece[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Greece" & fulldataset$Year == 2003+i] <- as.numeric(mf_greece[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Greece" & fulldataset$Year == 2003+i] <- as.numeric(mf_greece[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Greece" & fulldataset$Year == 2003+i] <- as.numeric(mf_greece[4,1+i])/100
}

#Hungary
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Hungary" & fulldataset$Year == 2003+i] <- as.numeric(mf_hungary[1,1+i])
  fulldataset$UR[fulldataset$Country == "Hungary" & fulldataset$Year == 2003+i] <- as.numeric(mf_hungary[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Hungary" & fulldataset$Year == 2003+i] <- as.numeric(mf_hungary[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Hungary" & fulldataset$Year == 2003+i] <- as.numeric(mf_hungary[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Hungary" & fulldataset$Year == 2003+i] <- as.numeric(mf_hungary[4,1+i])/100
}

#Iceland
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Iceland" & fulldataset$Year == 2003+i] <- as.numeric(mf_iceland[1,1+i])
  fulldataset$UR[fulldataset$Country == "Iceland" & fulldataset$Year == 2003+i] <- as.numeric(mf_iceland[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Iceland" & fulldataset$Year == 2003+i] <- as.numeric(mf_iceland[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Iceland" & fulldataset$Year == 2003+i] <- as.numeric(mf_iceland[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Iceland" & fulldataset$Year == 2003+i] <- as.numeric(mf_iceland[4,1+i])/100
}

#Ireland
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Ireland" & fulldataset$Year == 2003+i] <- as.numeric(mf_ireland[1,1+i])
  fulldataset$UR[fulldataset$Country == "Ireland" & fulldataset$Year == 2003+i] <- as.numeric(mf_ireland[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Ireland" & fulldataset$Year == 2003+i] <- as.numeric(mf_ireland[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Ireland" & fulldataset$Year == 2003+i] <- as.numeric(mf_ireland[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Ireland" & fulldataset$Year == 2003+i] <- as.numeric(mf_ireland[4,1+i])/100
}

#Italy
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Italy" & fulldataset$Year == 2003+i] <- as.numeric(mf_italy[1,1+i])
  fulldataset$UR[fulldataset$Country == "Italy" & fulldataset$Year == 2003+i] <- as.numeric(mf_italy[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Italy" & fulldataset$Year == 2003+i] <- as.numeric(mf_italy[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Italy" & fulldataset$Year == 2003+i] <- as.numeric(mf_italy[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Italy" & fulldataset$Year == 2003+i] <- as.numeric(mf_italy[4,1+i])/100
}

#Latvia
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Latvia" & fulldataset$Year == 2003+i] <- as.numeric(mf_latvia[1,1+i])
  fulldataset$UR[fulldataset$Country == "Latvia" & fulldataset$Year == 2003+i] <- as.numeric(mf_latvia[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Latvia" & fulldataset$Year == 2003+i] <- as.numeric(mf_latvia[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Latvia" & fulldataset$Year == 2003+i] <- as.numeric(mf_latvia[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Latvia" & fulldataset$Year == 2003+i] <- as.numeric(mf_latvia[4,1+i])/100
}

#Lithuania
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Lithuania" & fulldataset$Year == 2003+i] <- as.numeric(mf_lithuania[1,1+i])
  fulldataset$UR[fulldataset$Country == "Lithuania" & fulldataset$Year == 2003+i] <- as.numeric(mf_lithuania[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Lithuania" & fulldataset$Year == 2003+i] <- as.numeric(mf_lithuania[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Lithuania" & fulldataset$Year == 2003+i] <- as.numeric(mf_lithuania[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Lithuania" & fulldataset$Year == 2003+i] <- as.numeric(mf_lithuania[4,1+i])/100
}

#Luxembourg
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Luxembourg" & fulldataset$Year == 2003+i] <- as.numeric(mf_luxembourg[1,1+i])
  fulldataset$UR[fulldataset$Country == "Luxembourg" & fulldataset$Year == 2003+i] <- as.numeric(mf_luxembourg[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Luxembourg" & fulldataset$Year == 2003+i] <- as.numeric(mf_luxembourg[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Luxembourg" & fulldataset$Year == 2003+i] <- as.numeric(mf_luxembourg[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Luxembourg" & fulldataset$Year == 2003+i] <- as.numeric(mf_luxembourg[4,1+i])/100
}

#Netherlands
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Netherlands" & fulldataset$Year == 2003+i] <- as.numeric(mf_netherlands[1,1+i])
  fulldataset$UR[fulldataset$Country == "Netherlands" & fulldataset$Year == 2003+i] <- as.numeric(mf_netherlands[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Netherlands" & fulldataset$Year == 2003+i] <- as.numeric(mf_netherlands[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Netherlands" & fulldataset$Year == 2003+i] <- as.numeric(mf_netherlands[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Netherlands" & fulldataset$Year == 2003+i] <- as.numeric(mf_netherlands[4,1+i])/100
}

#Norway
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Norway" & fulldataset$Year == 2003+i] <- as.numeric(mf_norway[1,1+i])
  fulldataset$UR[fulldataset$Country == "Norway" & fulldataset$Year == 2003+i] <- as.numeric(mf_norway[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Norway" & fulldataset$Year == 2003+i] <- as.numeric(mf_norway[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Norway" & fulldataset$Year == 2003+i] <- as.numeric(mf_norway[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Norway" & fulldataset$Year == 2003+i] <- as.numeric(mf_norway[4,1+i])/100
}

#Poland
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Poland" & fulldataset$Year == 2003+i] <- as.numeric(mf_poland[1,1+i])
  fulldataset$UR[fulldataset$Country == "Poland" & fulldataset$Year == 2003+i] <- as.numeric(mf_poland[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Poland" & fulldataset$Year == 2003+i] <- as.numeric(mf_poland[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Poland" & fulldataset$Year == 2003+i] <- as.numeric(mf_poland[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Poland" & fulldataset$Year == 2003+i] <- as.numeric(mf_poland[4,1+i])/100
}

#Portugal
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Portugal" & fulldataset$Year == 2003+i] <- as.numeric(mf_portugal[1,1+i])
  fulldataset$UR[fulldataset$Country == "Portugal" & fulldataset$Year == 2003+i] <- as.numeric(mf_portugal[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Portugal" & fulldataset$Year == 2003+i] <- as.numeric(mf_portugal[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Portugal" & fulldataset$Year == 2003+i] <- as.numeric(mf_portugal[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Portugal" & fulldataset$Year == 2003+i] <- as.numeric(mf_portugal[4,1+i])/100
}

#Romania
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Romania" & fulldataset$Year == 2003+i] <- as.numeric(mf_romania[1,1+i])
  fulldataset$UR[fulldataset$Country == "Romania" & fulldataset$Year == 2003+i] <- as.numeric(mf_romania[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Romania" & fulldataset$Year == 2003+i] <- as.numeric(mf_romania[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Romania" & fulldataset$Year == 2003+i] <- as.numeric(mf_romania[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Romania" & fulldataset$Year == 2003+i] <- as.numeric(mf_romania[4,1+i])/100
}

#Serbia
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Serbia" & fulldataset$Year == 2003+i] <- as.numeric(mf_serbia[1,1+i])
  fulldataset$UR[fulldataset$Country == "Serbia" & fulldataset$Year == 2003+i] <- as.numeric(mf_serbia[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Serbia" & fulldataset$Year == 2003+i] <- as.numeric(mf_serbia[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Serbia" & fulldataset$Year == 2003+i] <- as.numeric(mf_serbia[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Serbia" & fulldataset$Year == 2003+i] <- as.numeric(mf_serbia[4,1+i])/100
}

#Slovak Republic
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Slovak Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovakrepublic[1,1+i])
  fulldataset$UR[fulldataset$Country == "Slovak Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovakrepublic[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Slovak Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovakrepublic[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Slovak Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovakrepublic[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Slovak Republic" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovakrepublic[4,1+i])/100
}

#Slovenia
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Slovenia" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovenia[1,1+i])
  fulldataset$UR[fulldataset$Country == "Slovenia" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovenia[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Slovenia" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovenia[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Slovenia" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovenia[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Slovenia" & fulldataset$Year == 2003+i] <- as.numeric(mf_slovenia[4,1+i])/100
}

#Spain
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Spain" & fulldataset$Year == 2003+i] <- as.numeric(mf_spain[1,1+i])
  fulldataset$UR[fulldataset$Country == "Spain" & fulldataset$Year == 2003+i] <- as.numeric(mf_spain[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Spain" & fulldataset$Year == 2003+i] <- as.numeric(mf_spain[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Spain" & fulldataset$Year == 2003+i] <- as.numeric(mf_spain[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Spain" & fulldataset$Year == 2003+i] <- as.numeric(mf_spain[4,1+i])/100
}

#Sweden
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Sweden" & fulldataset$Year == 2003+i] <- as.numeric(mf_sweden[1,1+i])
  fulldataset$UR[fulldataset$Country == "Sweden" & fulldataset$Year == 2003+i] <- as.numeric(mf_sweden[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Sweden" & fulldataset$Year == 2003+i] <- as.numeric(mf_sweden[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Sweden" & fulldataset$Year == 2003+i] <- as.numeric(mf_sweden[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Sweden" & fulldataset$Year == 2003+i] <- as.numeric(mf_sweden[4,1+i])/100
}

#Switzerland
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "Switzerland" & fulldataset$Year == 2003+i] <- as.numeric(mf_switzerland[1,1+i])
  fulldataset$UR[fulldataset$Country == "Switzerland" & fulldataset$Year == 2003+i] <- as.numeric(mf_switzerland[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "Switzerland" & fulldataset$Year == 2003+i] <- as.numeric(mf_switzerland[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "Switzerland" & fulldataset$Year == 2003+i] <- as.numeric(mf_switzerland[3,1+i])/100
  fulldataset$D[fulldataset$Country == "Switzerland" & fulldataset$Year == 2003+i] <- as.numeric(mf_switzerland[4,1+i])/100
}

#United Kingdom
for (i in 1:17){
  fulldataset$GDP[fulldataset$Country == "United Kingdom" & fulldataset$Year == 2003+i] <- as.numeric(mf_uk[1,1+i])
  fulldataset$UR[fulldataset$Country == "United Kingdom" & fulldataset$Year == 2003+i] <- as.numeric(mf_uk[5,1+i])/100
  fulldataset$CPG[fulldataset$Country == "United Kingdom" & fulldataset$Year == 2003+i] <- as.numeric(mf_uk[2,1+i])/100
  fulldataset$IR[fulldataset$Country == "United Kingdom" & fulldataset$Year == 2003+i] <- as.numeric(mf_uk[3,1+i])/100
  fulldataset$D[fulldataset$Country == "United Kingdom" & fulldataset$Year == 2003+i] <- as.numeric(mf_uk[4,1+i])/100
}
#------

#Full Data Summary
#------
summary(fulldataset)
str(fulldataset)

summary(fulldataset$Country) #32 negara - 1 negara (Malta)
summary(fulldataset$Sector) #14 sectors - 2 sectors (Edu and Finance)
summary(fulldataset$Year) #(2005-2020)
#------
