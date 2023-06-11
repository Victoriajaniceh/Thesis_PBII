#Poisson Regression Model

#install.packages('Hmisc') #Binning
#install.packages('performance') #Overdispersion

library(readxl)
library(writexl)
library(dplyr)
library(Hmisc)
library(MASS)
library(performance) #Overdispersion

#Input Data
#-----
setwd("/Users/victoriajanice/Documents/U N I/Thesis/Dataset")
df_poi <- read_xlsx("fulldatasetfixnew.xlsx")
#df_poi <- df_poi[-4] #drop DR column
#-----

#Data Split
#-----
#Data train 2005-2017
#Data test: 2018-2020
data_train_poi <- subset(df_poi,Year >= "2005" & Year <= "2017")
data_test_poi <- subset(df_poi,Year >= "2018" & Year < "2021")

##Drop unused columns
data_train_poi <- data_train_poi[-c(3)]
data_test_poi1 <- data_test_poi[-c(3)]
data_test_poi2 <- data_test_poi[-c(1,3)]

#Data for predictions -> for calculating payoff
fulldatatest_PR <- df_poi[-c(3,6)]
#-----

#Model Poisson Regression
#-----
#Dengan variabel negara !Masi harus buat data test pisah!
poi_model1 <- glm(ND ~ Country + Sector + BR + Prod + GDP + UR + CPG + IR + D, data_train_poi, family = poisson(link = "log"))
summary(poi_model1)
#predict_PR1 <- predict(poi_model1, data_test_poi1, type = "response")
#res_PR1 <- resid(poi_model1)

#Tanpa variabel negara 
poi_model2 <- glm(ND ~ Sector + BR + Prod + GDP + UR + CPG + IR + D, data_train_poi, family = poisson(link = "log"))
summary(poi_model2)
#predict_PR2 <- predict(poi_model2, data_test_poi2, type = "response")
#res_PR2 <- resid(poi_model2)
#-----

#Cek overdispersi
#-----
check_overdispersion(poi_model1)
check_overdispersion(poi_model2)
#-----

#Cek Independency
#----
# Obtain the residuals
residuals_1 <- residuals(poi_model1, type = "pearson")
residuals_2 <- residuals(poi_model2, type = "pearson")

# Check for independence using a scatterplot of residuals against fitted values
par(mfrow = c(1, 2))
plot(poi_model1$fitted.values, residuals_1, xlab = "Fitted Values", ylab = "Residual", main = "Scatterplot Residual Model Dengan Negara")
abline(h = 0, lty = 2)

plot(poi_model2$fitted.values, residuals_2, xlab = "Fitted Values", ylab = "Residual", main = "Scatterplot Residual Model Tanpa Negara")
abline(h = 0, lty = 2)
#----

#Model Quasi Poisson Regression
#-----
#Dengan variabel negara !
QP_model1 <- glm(ND ~ Country + Sector + BR + Prod + GDP + UR + CPG + IR + D, data_train_poi, family = quasipoisson())
summary(QP_model1)

QP_model1 <- glm(ND ~ Country + Sector + UR + D, data_train_poi, family = quasipoisson())
summary(QP_model1)

#Predict with full dataset
predict_QP1 <- predict(QP_model1, fulldatatest_PR, type = "response")
head(predict_QP1)

#Make into DR
predict_QP1 <- predict_QP1/df_poi$Pop

#Tanpa variabel negara 
QP_model2 <- glm(ND ~ Sector + BR + Prod + GDP + UR + CPG + IR + D, data = data_train_poi, family = quasipoisson())
summary(QP_model2)

#Final model (significant)
QP_model2 <- glm(ND ~ Sector + BR + Prod + GDP + UR + IR + D, data = data_train_poi, family = quasipoisson())
summary(QP_model2)

#Predict with full dataset
predict_QP2 <- predict(QP_model2, fulldatatest_PR, type = "response")
head(predict_QP2)

#Make into DR
predict_QP2 <- predict_QP2/df_poi$Pop
#-----

#Model NB Regression
#-----
#Dengan variabel negara !
NB_model1 <- glm.nb(ND ~ Country + Sector + BR + Prod + GDP + UR + CPG + IR + D, data_train_poi)
summary(NB_model1)

#Final model (significant)
NB_model1 <- glm.nb(ND ~ Country + Sector + BR + Prod + GDP, data_train_poi)
summary(NB_model1)

#Predict with full dataset
predict_NBR1 <- predict(NB_model1, fulldatatest_PR, type = "response")
head(predict_NBR1)

#Make into DR
predict_NBR1 <- predict_NBR1/df_poi$Pop

#Tanpa variabel negara 
NB_model2 <- glm.nb(ND ~ Sector + BR + Prod + GDP + UR + CPG + IR + D, data = data_train_poi)
summary(NB_model2)

#Final model (significant)
NB_model2 <- glm.nb(ND ~ Sector + BR + Prod + GDP + UR + IR + D, data = data_train_poi)
summary(NB_model2)

#Predict with full dataset
predict_NBR2 <- predict(NB_model2, fulldatatest_PR, type = "response")
head(predict_NBR2)

#Make into DR
predict_NBR2 <- predict_NBR2/df_poi$Pop
#-----

#Create table of deviance between model
#-----
deviancetable <- data.frame(Model = c("Negative Binomial 1", "Negative Binomial 2", "Quasi-Poisson 1", "Quasi-Poisson 2"), Deviance = 1:4)

deviancetable[1,2] <- deviance(NB_model1)
deviancetable[2,2] <- deviance(NB_model2)
deviancetable[3,2] <- deviance(QP_model1)
deviancetable[4,2] <- deviance(QP_model2)

write_xlsx(deviancetable, "Hasil Tabel Residual Deviance.xlsx")
#-----


#Check NB Histogram
#-----
library(MASS)

truehist(df_poi$ND, col = "steelblue") 
?truehist

lines(density(df_poi$ND), col = "red") 
#KDE (kernel density estimation) = a non-parametric method to estimate the probability density function of a random variable based on kernels as weights

?density

#By country
for (c in unique(df_poi$Country)) {
  # subset the data frame by country
  subset_df <- df_poi[df_poi$Country == c,]
  
  # Create a new PDF file for each plot
  pdf(paste0("Histogram of ND for ", c, ".pdf"))
  
  # create histogram
  title <- paste("Histogram of ND for", c)
  truehist(subset_df$ND, col = "steelblue", main = title)
  lines(density(subset_df$ND), col = "red") 
  
  # Close the PDF file
  dev.off()
}

#By sector
for (s in unique(df_poi$Sector)) {
  # subset the data frame by sector
  subset_df <- df_poi[df_poi$Sector == s,]
  
  # Create a new PDF file for each plot
  pdf(paste0("Histogram of ND for ", s, ".pdf"))
  
  # create histogram
  title <- paste("Histogram of ND for", s)
  truehist(subset_df$ND, col = "steelblue", main = title)
  lines(density(subset_df$ND), col = "red") 
  
  # Close the PDF file
  dev.off()
}

#hist only by frequency

#By country
for (c in unique(df_poi$Country)) {
  # subset the data frame by country
  subset_df <- df_poi[df_poi$Country == c,]
  
  # Create a new PDF file for each plot
  pdf(paste0("Frequency Histogram of ND for ", c, ".pdf"))
  
  # create histogram
  title <- paste("Histogram of ND for", c)
  hist(subset_df$ND, main = title)
  
  # Close the PDF file
  dev.off()
}

#By sector
for (s in unique(df_poi$Sector)) {
  # subset the data frame by sector
  subset_df <- df_poi[df_poi$Sector == s,]
  
  # Create a new PDF file for each plot
  pdf(paste0("Frequency Histogram of ND for ", s, ".pdf"))
  
  # create histogram
  title <- paste("Histogram of ND for", s)
  hist(subset_df$ND, main = title)
  
  # Close the PDF file
  dev.off()
}
#-----


