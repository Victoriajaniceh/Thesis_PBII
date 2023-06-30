#ANALISIS PERBANDINGAN MODEL

#New data
#----
df_new <- read_xlsx("fulldatasetfixnew.xlsx")

#LR with WOE
df_new <- cbind(df_new, df[,33:36])

data_train_new <- subset(df_new,Year >= "2005" & Year <= "2018")
data_test_new <- subset(df_new,Year >= "2019" & Year < "2021")
#----

#Calculate PSI
#----
library(readr)

predictors <- colnames(df_new)[c(5,8:13)]
#target years
data_2018 <- subset(df_new,Year == "2018")
data_2019 <- subset(df_new,Year == "2019")
data_2020 <- subset(df_new,Year == "2020")
data_rest <- subset(df_new,Year <= "2017") #base year

calculate_psi <- function(actual, predicted, bins) {
  # Calculate the expected and observed proportions
  expected <- (cut2(predicted, g = bins))
  
  # Extract the binning information
  bin_labels <- levels(expected)  # Get the labels of the bins
  
  bin_breaks <- as.numeric(gsub("\\[|,.*", "", bin_labels))  # Extract the lower bound of each bin
  bin_upper_last <- as.numeric(parse_number(gsub(".*,", "", tail(bin_labels, 1))))  # Extract the upper bound of the last bin
  bin_breaks <- c(bin_breaks, bin_upper_last)  # Add the upper bound to the bin breaks
  
  
  observed <- table(cut(actual, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE))
  expected <- table(cut2(predicted, g = bins))
  
  # Calculate the expected and observed proportions
  expected_prop <- prop.table(expected)
  observed_prop <- prop.table(observed)
  
  # Calculate the PSI value
  psi <- sum((observed_prop - expected_prop) * log(observed_prop / expected_prop))
  
  return(psi)
}
#Kalo PSI rendah berarti data stabil -> biasa ga perlu update model

#Table
PSI_Table <- data.frame(Variable = predictors, PSI2018 = NA, PSI2019 = NA, PSI2020 = NA)

PSI_Table[1,2] <-calculate_psi(data_2018$BR,data_rest$BR, bins = 5) 
PSI_Table[2,2] <-calculate_psi(data_2018$Prod,data_rest$Prod, bins = 5) 
PSI_Table[3,2] <-calculate_psi(data_2018$GDP,data_rest$GDP, bins = 5) 
PSI_Table[4,2] <-calculate_psi(data_2018$UR,data_rest$UR, bins = 5) 
PSI_Table[5,2] <-calculate_psi(data_2018$CPG,data_rest$CPG, bins = 2) 
PSI_Table[6,2] <-calculate_psi(data_2018$IR,data_rest$IR, bins = 3) 
PSI_Table[7,2] <-calculate_psi(data_2018$D,data_rest$D, bins = 5) 

PSI_Table[1,3] <-calculate_psi(data_2019$BR,data_rest$BR, bins = 5) 
PSI_Table[2,3] <-calculate_psi(data_2019$Prod,data_rest$Prod, bins = 5) 
PSI_Table[3,3] <-calculate_psi(data_2019$GDP,data_rest$GDP, bins = 5) 
PSI_Table[4,3] <-calculate_psi(data_2019$UR,data_rest$UR, bins = 5) 
PSI_Table[5,3] <-calculate_psi(data_2019$CPG,data_rest$CPG, bins = 2) #4 bins, 3 also can
PSI_Table[6,3] <-calculate_psi(data_2019$IR,data_rest$IR, bins = 3) #4 bins
PSI_Table[7,3] <-calculate_psi(data_2019$D,data_rest$D, bins = 5) 

PSI_Table[1,4] <-calculate_psi(data_2020$BR,data_rest$BR, bins = 5) 
PSI_Table[2,4] <-calculate_psi(data_2020$Prod,data_rest$Prod, bins = 5) 
PSI_Table[3,4] <-calculate_psi(data_2020$GDP,data_rest$GDP, bins = 5) 
PSI_Table[4,4] <-calculate_psi(data_2020$UR,data_rest$UR, bins = 5) 
PSI_Table[5,4] <-calculate_psi(data_2020$CPG,data_rest$CPG, bins = 2) 
PSI_Table[6,4] <-calculate_psi(data_2020$IR,data_rest$IR, bins = 3)  #3 bins
PSI_Table[7,4] <-calculate_psi(data_2020$D,data_rest$D, bins = 5) 

write_xlsx(PSI_Table, "Hasil Tabel Perhitungan PSI.xlsx")
#----

#Check CPG2018
#----
min(data_rest$CPG)
min(data_2018$CPG)

mean(data_rest$CPG)
mean(data_2018$CPG)

expected <- (cut2(data_rest$CPG, g = 2))

# Extract the binning information
bin_labels <- levels(expected)  # Get the labels of the bins

bin_breaks <- as.numeric(gsub("\\[|,.*", "", bin_labels))  # Extract the lower bound of each bin
bin_upper_last <- as.numeric(parse_number(gsub(".*,", "", tail(bin_labels, 1))))  # Extract the upper bound of the last bin
bin_breaks <- c(bin_breaks, bin_upper_last)  # Add the upper bound to the bin breaks

observed <- table(cut(data_2018$CPG, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE))
new_value <- "1"

expected <- table(cut2(data_rest$CPG, g = 2))

# Calculate the expected and observed proportions
expected_prop <- prop.table(expected)
observed_prop <- prop.table(observed)

# Calculate the PSI value
psi <- sum((observed_prop - expected_prop) * log(observed_prop / expected_prop))
#----

#Check CPG2019
#----
min(data_rest$CPG)
min(data_2019$CPG)

mean(data_rest$CPG)
mean(data_2019$CPG)

expected <- (cut2(data_rest$CPG, g = 4))

# Extract the binning information
bin_labels <- levels(expected)  # Get the labels of the bins

bin_breaks <- as.numeric(gsub("\\[|,.*", "", bin_labels))  # Extract the lower bound of each bin
bin_upper_last <- as.numeric(parse_number(gsub(".*,", "", tail(bin_labels, 1))))  # Extract the upper bound of the last bin
bin_breaks <- c(bin_breaks, bin_upper_last)  # Add the upper bound to the bin breaks

observed <- table(cut(data_2019$CPG, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE))
expected <- table(cut2(data_rest$CPG, g = 4))

# Calculate the expected and observed proportions
expected_prop <- prop.table(expected)
observed_prop <- prop.table(observed)

# Calculate the PSI value
PSI_Table[5,3] <- sum((observed_prop - expected_prop) * log(observed_prop / expected_prop))
#----

#Check IR2019
#----
expected <- (cut2(data_rest$IR, g = 4))

# Extract the binning information
bin_labels <- levels(expected)  # Get the labels of the bins

bin_breaks <- as.numeric(gsub("\\[|,.*", "", bin_labels))  # Extract the lower bound of each bin
bin_upper_last <- as.numeric(parse_number(gsub(".*,", "", tail(bin_labels, 1))))  # Extract the upper bound of the last bin
bin_breaks <- c(bin_breaks, bin_upper_last)  # Add the upper bound to the bin breaks

observed <- table(cut(data_2019$IR, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE))
expected <- table(cut2(data_rest$IR, g = 4))

# Calculate the expected and observed proportions
expected_prop <- prop.table(expected)
observed_prop <- prop.table(observed)

# Calculate the PSI value
PSI_Table[6,3] <- sum((observed_prop - expected_prop) * log(observed_prop / expected_prop))
#----

#Check IR2020
#----
min(data_rest$IR)
min(data_2020$IR)

mean(data_rest$IR)
mean(data_2020$IR)

expected <- (cut2(data_rest$IR, g = 3))

# Extract the binning information
bin_labels <- levels(expected)  # Get the labels of the bins

bin_breaks <- as.numeric(gsub("\\[|,.*", "", bin_labels))  # Extract the lower bound of each bin
bin_upper_last <- as.numeric(parse_number(gsub(".*,", "", tail(bin_labels, 1))))  # Extract the upper bound of the last bin
bin_breaks <- c(bin_breaks, bin_upper_last)  # Add the upper bound to the bin breaks

observed <- table(cut(data_2020$IR, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE))
expected <- table(cut2(data_rest$IR, g = 3))

# Calculate the expected and observed proportions
expected_prop <- prop.table(expected)
observed_prop <- prop.table(observed)

# Calculate the PSI value
PSI_Table[6,4] <- sum((observed_prop - expected_prop) * log(observed_prop / expected_prop))
#----

#Count Regression Models
#----
#Model Quasi Poisson Regression
#Dengan variabel negara
QP_model1_new <- glm(ND ~ Country + Sector + BR + Prod + GDP + UR + CPG + IR + D, data_train_new, family = quasipoisson())
summary(QP_model1_new)

QP_model1_new <- glm(ND ~ Country + Sector + UR + D, data_train_new, family = quasipoisson())
summary(QP_model1_new)

#Tanpa variabel negara 
QP_model2_new <- glm(ND ~ Sector + BR + Prod + GDP + UR + CPG + IR + D, data = data_train_new, family = quasipoisson())
summary(QP_model2_new)

#Final model (significant)
QP_model2_new <- glm(ND ~ Sector + BR + Prod + GDP + UR + IR + D, data = data_train_new, family = quasipoisson())
summary(QP_model2_new)

#Model Negative Binomial Regression
NB_model1_new <- glm.nb(ND ~ Country + Sector + BR + Prod + GDP + UR + CPG + IR + D, data_train_new)
summary(NB_model1_new)

#Final model (significant)
NB_model1_new <- glm.nb(ND ~ Country + Sector + BR + Prod + GDP, data_train_new)
summary(NB_model1_new)

#Predict with full dataset
predict_NBR1_new <- predict(NB_model1_new, df_new, type = "response")
head(predict_NBR1_new)

#Make into DR
predict_NBR1_new <- predict_NBR1_new/df_new$Pop

#Tanpa variabel negara 
NB_model2_new <- glm.nb(ND ~ Sector + BR + Prod + GDP + UR + CPG + IR + D, data = data_train_new)
summary(NB_model2_new)

#Final model (significant)
NB_model2_new <- glm.nb(ND ~ Sector + BR + Prod + GDP + UR + IR + D, data = data_train_new)
summary(NB_model2_new)

#Predict with full dataset
predict_NBR2_new <- predict(NB_model2_new, df_new, type = "response")
head(predict_NBR2_new)

#Make into DR
predict_NBR2_new <- predict_NBR2_new/df_new$Pop

#Calculate deviance
deviancetable_new <- data.frame(Model = c("Negative Binomial 1", "Negative Binomial 2", "Quasi-Poisson 1", "Quasi-Poisson 2"), Deviance = 1:4)

deviancetable_new[1,2] <- deviance(NB_model1)
deviancetable_new[2,2] <- deviance(NB_model2)
deviancetable_new[3,2] <- deviance(QP_model1)
deviancetable_new[4,2] <- deviance(QP_model2)

write_xlsx(deviancetable, "Hasil Tabel Residual Deviance 2 (refreshed).xlsx")
#-----

#Logistic Regression
#-----
#data biner loop

dfLR_new <- df_new

binary <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(binary) <- c("Percentile", "y*")
for(i in 1:21){
  binary[i,1] <- (0.71-(0.01*i))*100
  binary[i,2] <- quantile(dfLR_new$DR, probs = 0.71-(0.01*i)) 
}

for(i in 1:21){
  dfLR_new[,i+13] <- ifelse(dfLR_new$DR > binary[22-i,2], 1, 0)
}

##Rename columns
names(dfLR_new)[14:ncol(dfLR_new)]<- paste0(rep("v", each=21), "_",50:70)

##Looping

flags <- names(dfLR_new) 

#Dengan negara 
mod_summaries_1_new <- list()  
pred_summ_1_new <- data.frame(1:565)
Error_1_WOE_new <- as.data.frame(binary$Percentile)

for(i in 1:21) {  
  
  IV <- create_infotables(data=dfLR_new, y=flags[i+13], bins=5, parallel=FALSE)
  
  #GDP
  flag_GDP <- as.data.frame(cut2(dfLR_new$GDP, g=5))
  flag_GDP$binaryvar <- dfLR_new[,i+13]
  colnames(flag_GDP) <- c("Bins", "binaryvar")
  levels_GDP <- levels(flag_GDP$Bins)
  
  IV_Table_GDP <- IV$Tables$GDP
  IV_Table_GDP$GDP <- levels(flag_GDP$Bins)
  
  for (k in levels_GDP){
    flag_GDP$WOE_GDP[flag_GDP$Bins == k] <- IV_Table_GDP$WOE[IV_Table_GDP$GDP == k]
  }
  
  #CPG
  flag_CPG <- as.data.frame(cut2(dfLR_new$CPG, g=5))
  flag_CPG$binaryvar <- dfLR_new[,i+13]
  colnames(flag_CPG) <- c("Bins", "binaryvar")
  levels_CPG <- levels(flag_CPG$Bins)
  
  IV_Table_CPG <- IV$Tables$CPG
  IV_Table_CPG$CPG <- levels(flag_CPG$Bins)
  
  for (k in levels_CPG){
    flag_CPG$WOE_CPG[flag_CPG$Bins == k] <- IV_Table_CPG$WOE[IV_Table_CPG$CPG == k]
  }
  
  #IR
  flag_IR <- as.data.frame(cut2(dfLR_new$IR, g=5))
  flag_IR$binaryvar <- dfLR_new[,i+13]
  colnames(flag_IR) <- c("Bins", "binaryvar")
  levels_IR <- levels(flag_IR$Bins)
  
  IV_Table_IR <- IV$Tables$IR
  IV_Table_IR$IR <- levels(flag_IR$Bins)
  
  for (k in levels_IR){
    flag_IR$WOE_IR[flag_IR$Bins == k] <- IV_Table_IR$WOE[IV_Table_IR$IR == k]
  }
  
  #D
  flag_D <- as.data.frame(cut2(dfLR_new$D, g=5)) 
  flag_D$binaryvar <- dfLR_new[,i+13]
  colnames(flag_D) <- c("Bins", "binaryvar")
  levels_D <- levels(flag_D$Bins)
  
  IV_Table_D <- IV$Tables$D
  IV_Table_D$D <- levels(flag_D$Bins)
  
  for (k in levels_D){
    flag_D$WOE_D[flag_D$Bins == k] <- IV_Table_D$WOE[IV_Table_D$D == k]
  }
  
  #Input transformed variable
  dfLR_new$WOE_GDP <- flag_GDP$WOE_GDP
  dfLR_new$WOE_CPG <- flag_CPG$WOE_CPG
  dfLR_new$WOE_IR <- flag_IR$WOE_IR
  dfLR_new$WOE_D <- flag_D$WOE_D
  
  ##Data Splitting
  data_train <- subset(dfLR_new,Year >= "2005" & Year <= "2018")
  data_test <- subset(dfLR_new,Year >= "2019" & Year < "2021")
  
  ##Drop not needed columns
  data_train <- data_train[-c(3)]
  data_test1 <- data_test[-c(3,12:32)] #Dengan Negara
  data_test2 <- data_test[-c(1,3,12:32)] #Tanpa Negara
  
  mod_summaries_1_new[[i]] <- summary(glm(paste(flags[i+13], "~ Country + Sector + BR + Prod + WOE_GDP + UR + WOE_CPG + WOE_IR + WOE_D"), data = data_train, family = "binomial"))
  sig <- which(mod_summaries_1_new[[i]]$coefficients[43:49,4] < 0.05) #Smaller than alpha = 0.05
  sig_var <- as.character(names(sig))
  model <- glm(paste(flags[i+13], "~."), data = data_train[,c(paste(flags[i+13]), c("Country", "Sector", sig_var)) ], family = "binomial")
  mod_summaries_1_new[[i]] <- summary(model)
  predict <- predict(model, data_test1[,c("Country", "Sector", sig_var)], type = "response")
  predict <- ifelse(predict > binary[22-i,2], 1, 0)
  pred_summ_1_new <- cbind(pred_summ_1_new,as.data.frame(predict))
  #CF <- as.matrix(table(factor(as.matrix(data_test[,paste(flags[i+13])]), levels = c(0, 1)), factor(predict,levels = c(0, 1))))
  CF <- as.matrix(table(as.matrix(data_test[,paste(flags[i+13])]), predict))
  Error_1_WOE_new[22-i,2] <- (CF[1,2]+CF[2,1])/sum(CF)
}
names(pred_summ_1_new)[2:ncol(pred_summ_1_new)]<- paste0(rep("v", each=21), "_",50:70)
colnames(Error_1_WOE_new) <- c("Percentile", "Error")

Error_1_WOE_new[which(Error_1_WOE_new$Error == min(Error_1_WOE_new$Error)),] #Best Model is v_70
mod_summaries_1_new[[20]][["terms"]]

#Dengan negara 
glm_model1_new <- glm(v_69 ~ Country + Sector + BR + UR + WOE_CPG + WOE_IR, data = data_train, family = "binomial")
summary(glm_model1_new)

#Predict with full dataset
predict_LR1_new <- predict(glm_model1_new, dfLR_new, type = "response")

#Tanpa negara
mod_summaries_2_new <- list()  
pred_summ_2_new <- data.frame(1:565)
Error_2_WOE_new <- as.data.frame(binary$Percentile)

for(i in 1:21) {                
  IV <- create_infotables(data=dfLR_new, y=flags[i+13], bins=5, parallel=FALSE)
  
  #GDP
  flag_GDP <- as.data.frame(cut2(dfLR_new$GDP, g=5))
  flag_GDP$binaryvar <- dfLR_new[,i+13]
  colnames(flag_GDP) <- c("Bins", "binaryvar")
  levels_GDP <- levels(flag_GDP$Bins)
  
  IV_Table_GDP <- IV$Tables$GDP
  IV_Table_GDP$GDP <- levels(flag_GDP$Bins)
  
  for (k in levels_GDP){
    flag_GDP$WOE_GDP[flag_GDP$Bins == k] <- IV_Table_GDP$WOE[IV_Table_GDP$GDP == k]
  }
  
  #CPG
  flag_CPG <- as.data.frame(cut2(dfLR_new$CPG, g=5))
  flag_CPG$binaryvar <- dfLR_new[,i+13]
  colnames(flag_CPG) <- c("Bins", "binaryvar")
  levels_CPG <- levels(flag_CPG$Bins)
  
  IV_Table_CPG <- IV$Tables$CPG
  IV_Table_CPG$CPG <- levels(flag_CPG$Bins)
  
  for (k in levels_CPG){
    flag_CPG$WOE_CPG[flag_CPG$Bins == k] <- IV_Table_CPG$WOE[IV_Table_CPG$CPG == k]
  }
  
  #IR
  flag_IR <- as.data.frame(cut2(dfLR_new$IR, g=5))
  flag_IR$binaryvar <- dfLR_new[,i+13]
  colnames(flag_IR) <- c("Bins", "binaryvar")
  levels_IR <- levels(flag_IR$Bins)
  
  IV_Table_IR <- IV$Tables$IR
  IV_Table_IR$IR <- levels(flag_IR$Bins)
  
  for (k in levels_IR){
    flag_IR$WOE_IR[flag_IR$Bins == k] <- IV_Table_IR$WOE[IV_Table_IR$IR == k]
  }
  
  #D
  flag_D <- as.data.frame(cut2(dfLR_new$D, g=5)) 
  flag_D$binaryvar <- dfLR_new[,i+13]
  colnames(flag_D) <- c("Bins", "binaryvar")
  levels_D <- levels(flag_D$Bins)
  
  IV_Table_D <- IV$Tables$D
  IV_Table_D$D <- levels(flag_D$Bins)
  
  for (k in levels_D){
    flag_D$WOE_D[flag_D$Bins == k] <- IV_Table_D$WOE[IV_Table_D$D == k]
  }
  
  #Input transformed variable
  dfLR_new$WOE_GDP <- flag_GDP$WOE_GDP
  dfLR_new$WOE_CPG <- flag_CPG$WOE_CPG
  dfLR_new$WOE_IR <- flag_IR$WOE_IR
  dfLR_new$WOE_D <- flag_D$WOE_D
  
  ##Data Splitting
  data_train <- subset(dfLR_new,Year >= "2005" & Year <= "2018")
  data_test <- subset(dfLR_new,Year >= "2019" & Year < "2021")
  
  ##Drop not needed columns
  data_train <- data_train[-c(3)]
  data_test1 <- data_test[-c(3,12:32)] #Dengan Negara
  data_test2 <- data_test[-c(1,3,12:32)] #Tanpa Negara
  
  mod_summaries_2_new[[i]] <- summary(glm(paste(flags[i+13], "~ Sector + BR + Prod + WOE_GDP + UR + WOE_CPG + WOE_IR + WOE_D"), data = data_train, family = "binomial"))
  sig <- which(mod_summaries_2_new[[i]]$coefficients[13:19,4] < 0.05) #Smaller than alpha = 0.05
  sig_var <- as.character(names(sig))
  model <- glm(paste(flags[i+13], "~."), data = data_train[,c(paste(flags[i+13]), c("Sector", sig_var)) ], family = "binomial")
  mod_summaries_2_new[[i]] <- summary(model)
  predict <- predict(model, data_test2[,c("Sector", sig_var)], type = "response")
  predict <- ifelse(predict > binary[22-i,2], 1, 0)
  pred_summ_2_new <- cbind(pred_summ_2_new,as.data.frame(predict))
  CF <- as.matrix(table(as.matrix(data_test[,paste(flags[i+13])]), predict))
  Error_2_WOE_new[22-i,2] <- (CF[1,2]+CF[2,1])/sum(CF)
}
names(pred_summ_2_new)[2:ncol(pred_summ_2_new)]<- paste0(rep("v", each=21), "_",50:70)
colnames(Error_2_WOE_new) <- c("Percentile", "Error")

Error_2_WOE_new[which(Error_2_WOE_new$Error == min(Error_2_WOE_new$Error)),] #Best Model v_70
mod_summaries_2_new[[20]][["terms"]]

#Tanpa negara
glm_model2_new <- glm(v_69 ~ Sector + BR + WOE_GDP + UR + WOE_CPG + WOE_IR + WOE_D, data = data_train, family = "binomial")
summary(glm_model2_new)

predict_LR2_new <- predict(glm_model2_new, dfLR_new, type = "response")
#-----

#Regression Tree
#----
#Dengan negara
rt_model1_new <- rpart(DR ~ Country + Sector + BR + Prod + GDP + UR + CPG + IR + D, data=data_train_new, method = "anova")
#summary(rt_model1_new)
rpart.plot(rt_model1_new, type = 1, digits = 3, fallen.leaves = TRUE, cex = 0.7, compress = TRUE, ycompress = TRUE, faclen = -3)
#print(rt_model1_new, minlength = 0, spaces = 2, digits = getOption("digits"),nsmall = min(20, digits),)

#Predict with full dataset
predict_RT1_new <- predict(rt_model1_new, data_test_new)

var_imp_1_new <- data.frame(
  Variable = names(rt_model1_new$variable.importance),
  Importance = round(rt_model1_new$variable.importance,5)
)

var_imp_1_new %>%
  mutate(Variabel = factor(Variable, 
                           levels = Variable[order(var_imp_1_new$Importance)])) %>%
  ggplot(aes(x = Variabel, y = Importance)) + 
  geom_col() +
  coord_flip() +
  geom_text(aes(label = Importance), size = 3,hjust = +0.5) +
  ggtitle("Model Baru dengan Variabel Negara") +
  theme(plot.title = element_text(hjust = 0.5))

#Tanpa negara
rt_model2_new <- rpart(DR ~ Sector + BR + Prod + GDP + UR + CPG + IR + D, data=data_train_new, method = "anova")
#summary(rt_model2_new)
#rpart.plot(rt_model2_new, type = 3, digits = 3, fallen.leaves = TRUE)
rpart.plot(rt_model2_new, type = 1, digits = 4, fallen.leaves = TRUE, cex = 1, compress = TRUE, ycompress = TRUE, faclen = -3)
#print(rt_model2_new, minlength = 0, spaces = 2, digits = getOption("digits"), nsmall = min(20, digits),)

#Predict with full dataset
predict_RT2_new <- predict(rt_model2_new, data_test_new)
head(predict_RT2_new)

var_imp_2_new <- data.frame(
  Variable = names(rt_model2_new$variable.importance),
  Importance = round(rt_model2_new$variable.importance,5)
)

var_imp_2_new %>%
  mutate(Variabel = factor(Variable, 
                           levels = Variable[order(var_imp_2_new$Importance)])) %>%
  ggplot(aes(x = Variabel, y = Importance)) + 
  geom_col() +
  coord_flip() +
  geom_text(aes(label = Importance), size = 3,hjust = +0.5) +
  ggtitle("Model Baru tanpa Variabel Negara") +
  theme(plot.title = element_text(hjust = 0.5))

#----

#Hitung MSE MAE Mean Y
#----
#Model 1
#NBR
predict_NBR1_2019_old <- predict(NB_model1, data_2019, type = "response")/(data_2019$Pop)
predict_NBR1_2019_new <- predict(NB_model1_new, data_2019, type = "response")/(data_2019$Pop)
predict_NBR1_2020_old <- predict(NB_model1, data_2020, type = "response")/(data_2020$Pop)
predict_NBR1_2020_new <- predict(NB_model1_new, data_2020, type = "response")/(data_2020$Pop)

#LR
predict_LR1_2019_old <- predict(glm_model1, data_2019, type = "response")
predict_LR1_2019_new <- predict(glm_model1_new, data_2019, type = "response")
predict_LR1_2020_old <- predict(glm_model1, data_2020, type = "response")
predict_LR1_2020_new <- predict(glm_model1_new, data_2020, type = "response")

#RT
predict_RT1_2019_old <- predict(rt_model1, data_2019)
predict_RT1_2019_new <- predict(rt_model1_new, data_2019)
predict_RT1_2020_old <- predict(rt_model1, data_2020)
predict_RT1_2020_new <- predict(rt_model1_new, data_2020)

wb_kasi_os <- createWorkbook()

#Add to sheets
addWorksheet(wb_kasi_os, sheetName = "NBROld19")
writeData(wb_kasi_os, sheet = "NBROld19", x = predict_NBR1_2019_old)

addWorksheet(wb_kasi_os, sheetName = "NBRNew19")
writeData(wb_kasi_os, sheet = "NBRNew19", x = predict_NBR1_2019_new)


# save workbook to file
saveWorkbook(wb_kasi_os, "Tabel Hasil Analisa MSE MAE Lengkap New Revised.xlsx", overwrite = FALSE)

#NBR Table
NBR_Err_N <- data.frame(Year = c(2019,2020), Mean_Y = NA, OldMAE = NA, NewMAE = NA, OldMSE = NA, NewMSE = NA)
#Mean
NBR_Err_N$Mean_Y[1] <- mean(data_2019$DR)
NBR_Err_N$Mean_Y[2] <- mean(data_2020$DR)
#MAE
NBR_Err_N$OldMAE[1] <- mean(abs(predict_NBR1_2019_old - data_2019$DR))
NBR_Err_N$NewMAE[1] <- mean(abs(predict_NBR1_2019_new - data_2019$DR))
NBR_Err_N$OldMAE[2] <- mean(abs(predict_NBR1_2020_old - data_2020$DR))
NBR_Err_N$NewMAE[2] <- mean(abs(predict_NBR1_2020_new - data_2020$DR))
#MSE
NBR_Err_N$OldMSE[1] <- mean((predict_NBR1_2019_old - data_2019$DR)^2)
NBR_Err_N$NewMSE[1] <- mean((predict_NBR1_2019_new - data_2019$DR)^2)
NBR_Err_N$OldMSE[2] <- mean((predict_NBR1_2020_old - data_2020$DR)^2)
NBR_Err_N$NewMSE[2] <- mean((predict_NBR1_2020_new - data_2020$DR)^2)

#Logreg Table
LR_Err_N <- data.frame(Year = c(2019,2020), Mean_Y = NA, OldMAE = NA, NewMAE = NA, OldMSE = NA, NewMSE = NA)
#Mean
LR_Err_N$Mean_Y[1] <- mean(data_2019$DR)
LR_Err_N$Mean_Y[2] <- mean(data_2020$DR)
#MAE
LR_Err_N$OldMAE[1] <- mean(abs(predict_LR1_2019_old - data_2019$DR))
LR_Err_N$NewMAE[1] <- mean(abs(predict_LR1_2019_new - data_2019$DR))
LR_Err_N$OldMAE[2] <- mean(abs(predict_LR1_2020_old - data_2020$DR))
LR_Err_N$NewMAE[2] <- mean(abs(predict_LR1_2020_new - data_2020$DR))
#MSE
LR_Err_N$OldMSE[1] <- mean((predict_LR1_2019_old - data_2019$DR)^2)
LR_Err_N$NewMSE[1] <- mean((predict_LR1_2019_new - data_2019$DR)^2)
LR_Err_N$OldMSE[2] <- mean((predict_LR1_2020_old - data_2020$DR)^2)
LR_Err_N$NewMSE[2] <- mean((predict_LR1_2020_new - data_2020$DR)^2)

#RT Table
RT_Err_N <- data.frame(Year = c(2019,2020), Mean_Y = NA, OldMAE = NA, NewMAE = NA, OldMSE = NA, NewMSE = NA)
#Mean
RT_Err_N$Mean_Y[1] <- mean(data_2019$DR)
RT_Err_N$Mean_Y[2] <- mean(data_2020$DR)
#MAE
RT_Err_N$OldMAE[1] <- mean(abs(predict_RT1_2019_old - data_2019$DR))
RT_Err_N$NewMAE[1] <- mean(abs(predict_RT1_2019_new - data_2019$DR))
RT_Err_N$OldMAE[2] <- mean(abs(predict_RT1_2020_old - data_2020$DR))
RT_Err_N$NewMAE[2] <- mean(abs(predict_RT1_2020_new - data_2020$DR))
#MSE
RT_Err_N$OldMSE[1] <- mean((predict_RT1_2019_old - data_2019$DR)^2)
RT_Err_N$NewMSE[1] <- mean((predict_RT1_2019_new - data_2019$DR)^2)
RT_Err_N$OldMSE[2] <- mean((predict_RT1_2020_old - data_2020$DR)^2)
RT_Err_N$NewMSE[2] <- mean((predict_RT1_2020_new - data_2020$DR)^2)

#Make to excel
# create new workbook
wb_err <- createWorkbook()

#Add to sheets
addWorksheet(wb_err, sheetName = "NBR")
writeData(wb_err, sheet = "NBR", x = NBR_Err_N)

addWorksheet(wb_err, sheetName = "LR")
writeData(wb_err, sheet = "LR", x = LR_Err_N)

addWorksheet(wb_err, sheetName = "RT")
writeData(wb_err, sheet = "RT", x = RT_Err_N)

# save workbook to file
saveWorkbook(wb_err, "Tabel Hasil Analisa MSE MAE Tahunan Revised.xlsx", overwrite = TRUE)

##Model 2
#NBR
predict_NBR2_2019_old <- predict(NB_model2, data_2019, type = "response")/(data_2019$Pop)
predict_NBR2_2019_new <- predict(NB_model2_new, data_2019, type = "response")/(data_2019$Pop)
predict_NBR2_2020_old <- predict(NB_model2, data_2020, type = "response")/(data_2020$Pop)
predict_NBR2_2020_new <- predict(NB_model2_new, data_2020, type = "response")/(data_2020$Pop)

#LR
predict_LR2_2019_old <- predict(glm_model2, data_2019, type = "response")
predict_LR2_2019_new <- predict(glm_model2_new, data_2019, type = "response")
predict_LR2_2020_old <- predict(glm_model2, data_2020, type = "response")
predict_LR2_2020_new <- predict(glm_model2_new, data_2020, type = "response")

#RT
predict_RT2_2019_old <- predict(rt_model2, data_2019)
predict_RT2_2019_new <- predict(rt_model2_new, data_2019)
predict_RT2_2020_old <- predict(rt_model2, data_2020)
predict_RT2_2020_new <- predict(rt_model2_new, data_2020)

#NBR Table
NBR_Err_N2 <- data.frame(Year = c(2019,2020), Mean_Y = NA, OldMAE = NA, NewMAE = NA, OldMSE = NA, NewMSE = NA)
#Mean
NBR_Err_N2$Mean_Y[1] <- mean(data_2019$DR)
NBR_Err_N2$Mean_Y[2] <- mean(data_2020$DR)
#MAE
NBR_Err_N2$OldMAE[1] <- mean(abs(predict_NBR2_2019_old - data_2019$DR))
NBR_Err_N2$NewMAE[1] <- mean(abs(predict_NBR2_2019_new - data_2019$DR))
NBR_Err_N2$OldMAE[2] <- mean(abs(predict_NBR2_2020_old - data_2020$DR))
NBR_Err_N2$NewMAE[2] <- mean(abs(predict_NBR2_2020_new - data_2020$DR))
#MSE
NBR_Err_N2$OldMSE[1] <- mean((predict_NBR2_2019_old - data_2019$DR)^2)
NBR_Err_N2$NewMSE[1] <- mean((predict_NBR2_2019_new - data_2019$DR)^2)
NBR_Err_N2$OldMSE[2] <- mean((predict_NBR2_2020_old - data_2020$DR)^2)
NBR_Err_N2$NewMSE[2] <- mean((predict_NBR2_2020_new - data_2020$DR)^2)

#Logreg Table
LR_Err_N2 <- data.frame(Year = c(2019,2020), Mean_Y = NA, OldMAE = NA, NewMAE = NA, OldMSE = NA, NewMSE = NA)
#Mean
LR_Err_N2$Mean_Y[1] <- mean(data_2019$DR)
LR_Err_N2$Mean_Y[2] <- mean(data_2020$DR)
#MAE
LR_Err_N2$OldMAE[1] <- mean(abs(predict_LR2_2019_old - data_2019$DR))
LR_Err_N2$NewMAE[1] <- mean(abs(predict_LR2_2019_new - data_2019$DR))
LR_Err_N2$OldMAE[2] <- mean(abs(predict_LR2_2020_old - data_2020$DR))
LR_Err_N2$NewMAE[2] <- mean(abs(predict_LR2_2020_new - data_2020$DR))
#MSE
LR_Err_N2$OldMSE[1] <- mean((predict_LR2_2019_old - data_2019$DR)^2)
LR_Err_N2$NewMSE[1] <- mean((predict_LR2_2019_new - data_2019$DR)^2)
LR_Err_N2$OldMSE[2] <- mean((predict_LR2_2020_old - data_2020$DR)^2)
LR_Err_N2$NewMSE[2] <- mean((predict_LR2_2020_new - data_2020$DR)^2)

#RT Table
RT_Err_N2 <- data.frame(Year = c(2019,2020), Mean_Y = NA, OldMAE = NA, NewMAE = NA, OldMSE = NA, NewMSE = NA)
#Mean
RT_Err_N2$Mean_Y[1] <- mean(data_2019$DR)
RT_Err_N2$Mean_Y[2] <- mean(data_2020$DR)
#MAE
RT_Err_N2$OldMAE[1] <- mean(abs(predict_RT2_2019_old - data_2019$DR))
RT_Err_N2$NewMAE[1] <- mean(abs(predict_RT2_2019_new - data_2019$DR))
RT_Err_N2$OldMAE[2] <- mean(abs(predict_RT2_2020_old - data_2020$DR))
RT_Err_N2$NewMAE[2] <- mean(abs(predict_RT2_2020_new - data_2020$DR))
#MSE
RT_Err_N2$OldMSE[1] <- mean((predict_RT2_2019_old - data_2019$DR)^2)
RT_Err_N2$NewMSE[1] <- mean((predict_RT2_2019_new - data_2019$DR)^2)
RT_Err_N2$OldMSE[2] <- mean((predict_RT2_2020_old - data_2020$DR)^2)
RT_Err_N2$NewMSE[2] <- mean((predict_RT2_2020_new - data_2020$DR)^2)

#Make to excel
# create new workbook
wb_err2 <- createWorkbook()

#Add to sheets
addWorksheet(wb_err2, sheetName = "NBR")
writeData(wb_err2, sheet = "NBR", x = NBR_Err_N2)

addWorksheet(wb_err2, sheetName = "LR")
writeData(wb_err2, sheet = "LR", x = LR_Err_N2)

addWorksheet(wb_err2, sheetName = "RT")
writeData(wb_err2, sheet = "RT", x = RT_Err_N2)

# save workbook to file
saveWorkbook(wb_err2, "Tabel Hasil Analisa MSE MAE Tahunan model 2 LR nya aja.xlsx", overwrite = TRUE)
#----

#Full data Predictions
#------
Predictions_new <- as.data.frame(1:4283)
Predictions_new$`1:4283` <- 1:4283
Predictions_new$DR <- df_new$DR

Predictions_new$LR1 <- predict(glm_model1_new, df_new, type = "response")
Predictions_new$LR2 <- predict(glm_model2_new, df_new, type = "response")

Predictions_new$NBR1 <- (predict(NB_model1_new, df_new, type = "response"))/(df_new$Pop)
Predictions_new$NBR2 <- (predict(NB_model2_new, df_new, type = "response"))/(df_new$Pop)

Predictions_new$RT1 <- predict(rt_model1_new, df_new)
Predictions_new$RT2 <- predict(rt_model2_new, df_new)

colnames(Predictions_new) <- c("No", "DR","LR1"," LR2", "NBR1", "NBR2", "RT1", "RT2")
write_xlsx(Predictions_new, 'Predictions New model 2 refreshed.xlsx')
Predictions_new <- read_xlsx('Predictions New model 2 refreshed.xlsx')
#------

##Calculate Payoff
#------
#Create All dataframes for each sector and country
#df <- read_xlsx("fulldatasetfix.xlsx") sblm pakek 
dataframe_payoff_new <- df_new[,c(1:4,6:7)]
dataframe_payoff_new$LR <- Predictions_new$LR1
dataframe_payoff_new$NBR <- Predictions_new$NBR1
dataframe_payoff_new$RT <- Predictions_new$RT1
dataframe <- as.data.frame(NA)
lists <- list()
fulllist<- list()

countries <- unique(df_new$Country)
sectors <- unique(df_new$Sector)
Premi <- 1
COR <- 0.5
Exp <- 0.15

for (i in 1:32){
  for (j in 1:12){
    dataframe <- dataframe_payoff_new[dataframe_payoff_new$Country == countries[i] & dataframe_payoff_new$Sector == sectors[j], ]
    lists[[j]] <- dataframe
    lists[[j]]$Y_bar <- c(0,cumsum(lists[[j]]$DR)/seq(1:nrow(lists[[j]])))[-(nrow(lists[[j]])+1)]
    lists[[j]]$K_LR <- c(0,cumsum(lists[[j]]$DR)/seq(1:nrow(lists[[j]])))[-(nrow(lists[[j]])+1)]
    lists[[j]]$K_NBR <- c(0,cumsum(lists[[j]]$DR)/seq(1:nrow(lists[[j]])))[-(nrow(lists[[j]])+1)]
    lists[[j]]$K_RT <- c(0,cumsum(lists[[j]]$DR)/seq(1:nrow(lists[[j]])))[-(nrow(lists[[j]])+1)]
    lists[[j]]$y_Y_bar <- lists[[j]]$DR - lists[[j]]$Y_bar
    lists[[j]]$y_K_LR <- lists[[j]]$LR - lists[[j]]$K_LR
    lists[[j]]$y_K_NBR <- lists[[j]]$NBR - lists[[j]]$K_NBR
    lists[[j]]$y_K_RT <- lists[[j]]$RT - lists[[j]]$K_RT
    lists[[j]]$PM <- ifelse(lists[[j]]$y_Y_bar <= 0,0,lists[[j]]$y_Y_bar)
    lists[[j]]$PI_LR <- ifelse(lists[[j]]$y_K_LR <= 0,0,lists[[j]]$y_K_LR)
    lists[[j]]$PI_NBR <- ifelse(lists[[j]]$y_K_NBR <= 0,0,lists[[j]]$y_K_NBR)
    lists[[j]]$PI_RT <- ifelse(lists[[j]]$y_K_RT <= 0,0,lists[[j]]$y_K_RT)
    lists[[j]]$PM_bin <- ifelse(lists[[j]]$PM > 0, 1, 0)
    lists[[j]]$PI_LR_bin <- ifelse(lists[[j]]$PI_LR > 0, 1, 0)
    lists[[j]]$PI_NBR_bin <- ifelse(lists[[j]]$PI_NBR > 0, 1, 0)
    lists[[j]]$PI_RT_bin <- ifelse(lists[[j]]$PI_RT > 0, 1, 0)
    lists[[j]]$PM_CO <- ifelse(lists[[j]]$y_Y_bar <= 10^(-10),0,((Premi*COR)/(1+Exp))*(lists[[j]]$y_Y_bar*100))
    lists[[j]]$PI_LR_CO <- ifelse(lists[[j]]$y_K_LR <= 10^(-10),0,((Premi*COR)/(1+Exp))*(lists[[j]]$y_K_LR*100))
    lists[[j]]$PI_NBR_CO <- ifelse(lists[[j]]$y_K_NBR <= 10^(-10),0,((Premi*COR)/(1+Exp))*(lists[[j]]$y_K_NBR*100))
    lists[[j]]$PI_RT_CO <- ifelse(lists[[j]]$y_K_RT <= 10^(-10),0,((Premi*COR)/(1+Exp))*(lists[[j]]$y_K_RT*100))
    lists[[j]] <- lists[[j]][-1,]
  }
  fulllist <- c(fulllist,lists)
}

# Remove lists with data frames that have NA values
fulllist <- fulllist[sapply(fulllist, function(x) !any(is.na(x)))]

#Make to excel
# create new workbook
wb <- createWorkbook()

# loop through the list and add each dataframe as a new sheet
for (i in seq_along(fulllist)) {
  addWorksheet(wb, sheetName = paste0(fulllist[[i]]$Country[1], "_", substr(fulllist[[i]]$Sector[1],1,3)))
  writeData(wb, sheet = i, x = fulllist[[i]])
}

# save workbook to file
saveWorkbook(wb, "Tabel Payoff Negara dan Sektor Fix Model 2.xlsx", overwrite = TRUE)

#Buat df masing-masing
for (i in seq(fulllist)){
  assign(paste0("df_new_", fulllist[[i]]$Country[1], "_", fulllist[[i]]$Sector[1]), fulllist[[i]])
}
#------

#Confusion Matrix for Basis Risk (check best regression method for index insurance)
#-----
conf_matrix_new <- data.frame(Country = NA,Sector = NA, Year = NA, PM_bin =NA ,PI_LR_bin = NA, PI_NBR_bin= NA, PI_RT_bin = NA)

# Confusion Matrix for all data
for (i in (seq(fulllist))){
  cm <- fulllist[[i]][,c(1:3,22:25)]
  conf_matrix_new <- rbind(conf_matrix_new,cm)
}

conf_matrix_new <- na.omit(conf_matrix_new)
#Each country and sector
# create a list to store the confusion matrices
confusion_matrices_country_new <- list()

# loop through each unique combination ofcountry
for (c in unique(conf_matrix_new$Country)) {
  # subset the data frame by country
  subset_df <- conf_matrix_new[conf_matrix_new$Country == c,]
  
  # create a confusion matrix for var1 and var2
  confusion_matrix_LR <- table(factor(subset_df$PM_bin, levels = c(0, 1)), factor(subset_df$PI_LR_bin, levels = c(0, 1)))
  confusion_matrix_NBR <- table(actual = subset_df$PM_bin, predicted = subset_df$PI_NBR_bin)
  confusion_matrix_RT <- table(factor(subset_df$PM_bin, levels = c(0, 1)), factor(subset_df$PI_RT_bin, levels = c(0, 1)))
  
  # add the confusion matrix to the list
  confusion_matrices_country_new[[c]]$LR_BR1 <- confusion_matrix_LR[1,2]
  confusion_matrices_country_new[[c]]$LR_BR2 <- confusion_matrix_LR[2,1]
  confusion_matrices_country_new[[c]]$LR_P <- confusion_matrix_LR[2,2]
  confusion_matrices_country_new[[c]]$LR_TP <- confusion_matrix_LR[1,1]
  confusion_matrices_country_new[[c]]$NBR_BR1 <- confusion_matrix_NBR[1,2]
  confusion_matrices_country_new[[c]]$NBR_BR2 <- confusion_matrix_NBR[2,1]
  confusion_matrices_country_new[[c]]$NBR_P <- confusion_matrix_NBR[2,2]
  confusion_matrices_country_new[[c]]$NBR_TP <- confusion_matrix_NBR[1,1]
  confusion_matrices_country_new[[c]]$RT_BR1 <- confusion_matrix_RT[1,2]
  confusion_matrices_country_new[[c]]$RT_BR2 <- confusion_matrix_RT[2,1]
  confusion_matrices_country_new[[c]]$RT_P <- confusion_matrix_RT[2,2]
  confusion_matrices_country_new[[c]]$RT_TP <- confusion_matrix_RT[1,1]
}

#Count
LR_count_c <- 0
NBR_count_c <- 0
RT_count_c <- 0

# find best method for each country
for (i in seq(confusion_matrices_country_new)) {
  LR_Error <- confusion_matrices_country_new[[i]]$LR_BR1 + confusion_matrices_country_new[[i]]$LR_BR2
  NBR_Error <- confusion_matrices_country_new[[i]]$NBR_BR1 + confusion_matrices_country_new[[i]]$NBR_BR2
  RT_Error <- confusion_matrices_country_new[[i]]$RT_BR1 + confusion_matrices_country_new[[i]]$RT_BR2
  Best <- min(LR_Error,NBR_Error,RT_Error)
  if (Best == LR_Error){
    Best <- "Logistic Regression"
    LR_count_c <- 1 + LR_count_c
  } else if (Best  == NBR_Error){
    Best <- "Negative Binomial Regression"
    NBR_count_c <- 1 + NBR_count_c
  } else {
    Best <- "Regression Tree"
    RT_count_c <- 1 + RT_count_c
  }
  confusion_matrices_country_new[[i]]$Best_Method <- Best
}

# create a list to store the confusion matrices
confusion_matrices_sector_new <- list()

# loop through each unique combination of sector
for (s in unique(conf_matrix_new$Sector)) {
  # subset the data frame by sector
  subset_df <- conf_matrix_new[conf_matrix_new$Sector == s,]
  
  # create a confusion matrix for var1 and var2
  confusion_matrix_LR <- table(subset_df$PM_bin, subset_df$PI_LR_bin)
  confusion_matrix_NBR <- table(subset_df$PM_bin, subset_df$PI_NBR_bin)
  confusion_matrix_RT <- table(subset_df$PM_bin, subset_df$PI_RT_bin)
  
  # add the confusion matrix to the list
  # add the confusion matrix to the list
  confusion_matrices_sector_new[[s]]$LR_BR1 <- confusion_matrix_LR[1,2]
  confusion_matrices_sector_new[[s]]$LR_BR2 <- confusion_matrix_LR[2,1]
  confusion_matrices_sector_new[[s]]$LR_P <- confusion_matrix_LR[2,2]
  confusion_matrices_sector_new[[s]]$LR_TP <- confusion_matrix_LR[1,1]
  confusion_matrices_sector_new[[s]]$NBR_BR1 <- confusion_matrix_NBR[1,2]
  confusion_matrices_sector_new[[s]]$NBR_BR2 <- confusion_matrix_NBR[2,1]
  confusion_matrices_sector_new[[s]]$NBR_P <- confusion_matrix_NBR[2,2]
  confusion_matrices_sector_new[[s]]$NBR_TP <- confusion_matrix_NBR[1,1]
  confusion_matrices_sector_new[[s]]$RT_BR1 <- confusion_matrix_RT[1,2]
  confusion_matrices_sector_new[[s]]$RT_BR2 <- confusion_matrix_RT[2,1]
  confusion_matrices_sector_new[[s]]$RT_P <- confusion_matrix_RT[2,2]
  confusion_matrices_sector_new[[s]]$RT_TP <- confusion_matrix_RT[1,1]
}

#Count
LR_count_s <- 0
NBR_count_s <- 0
RT_count_s <- 0

# find best method for each sector
for (i in seq(confusion_matrices_sector_new)) {
  LR_Error <- confusion_matrices_sector_new[[i]]$LR_BR1 + confusion_matrices_sector_new[[i]]$LR_BR2
  NBR_Error <- confusion_matrices_sector_new[[i]]$NBR_BR1 + confusion_matrices_sector_new[[i]]$NBR_BR2
  RT_Error <- confusion_matrices_sector_new[[i]]$RT_BR1 + confusion_matrices_sector_new[[i]]$RT_BR2
  Best <- min(LR_Error,NBR_Error,RT_Error)
  if (Best == LR_Error){
    Best <- "Logistic Regression"
    LR_count_s <- 1 + LR_count_s
  } else if (Best  == NBR_Error){
    Best <- "Negative Binomial Regression"
    NBR_count_s <- 1 + NBR_count_s
  } else {
    Best <- "Regression Tree"
    RT_count_s <- 1 + RT_count_s
  }
  confusion_matrices_sector_new[[i]]$Best_Method <- Best
}

cm_table_country_new <- do.call(rbind, confusion_matrices_country_new)
cm_table_country_new <- cbind(Country = rownames(cm_table_country_new), cm_table_country_new)
cm_table_country_new <- as.data.frame(cm_table_country_new)

cm_table_sector_new <- do.call(rbind, confusion_matrices_sector_new)
cm_table_sector_new <- cbind(Sector = rownames(cm_table_sector_new), cm_table_sector_new)
cm_table_sector_new <- as.data.frame(cm_table_sector_new)

#Summarized Table
methodstable_new <- data.frame(
  methods = c("Logistic Regression", "Negative Binomial Regression", "Regression Tree"),
  Country = c(LR_count_c,NBR_count_c,RT_count_c),
  Sector = c(LR_count_s,NBR_count_s,RT_count_s)
)
#-----

#Calculate F1 Score
#----
Metric_country_new <- data.frame(Country = 1:31, PresLR = NA, PresNBR = NA, PresRT = NA, SenLR = NA, SenNBR = NA, SenRT = NA, F1LR = NA, F1NBR = NA, F1RT = NA, Best_Method = NA)
Metric_sector_new <- data.frame(Sector = 1:12, PresLR = NA, PresNBR = NA, PresRT = NA, SenLR = NA, SenNBR = NA, SenRT = NA, F1LR = NA, F1NBR = NA, F1RT = NA, Best_Method = NA)

Metric_country_new$Country <- cm_table_country_new$Country
Metric_sector_new$Sector <- cm_table_sector_new$Sector

#Calculate Precision
Metric_country_new$PresLR <- (as.numeric(cm_table_country_new$LR_P)/(as.numeric(cm_table_country_new$LR_P) + as.numeric(cm_table_country_new$LR_BR1)))
Metric_country_new$PresNBR <- (as.numeric(cm_table_country_new$NBR_P)/(as.numeric(cm_table_country_new$NBR_P) + as.numeric(cm_table_country_new$NBR_BR1)))
Metric_country_new$PresRT <- (as.numeric(cm_table_country_new$RT_P)/(as.numeric(cm_table_country_new$RT_P) + as.numeric(cm_table_country_new$RT_BR1)))

Metric_sector_new$PresLR <- (as.numeric(cm_table_sector_new$LR_P)/(as.numeric(cm_table_sector_new$LR_P) + as.numeric(cm_table_sector_new$LR_BR1)))
Metric_sector_new$PresNBR <- (as.numeric(cm_table_sector_new$NBR_P)/(as.numeric(cm_table_sector_new$NBR_P) + as.numeric(cm_table_sector_new$NBR_BR1)))
Metric_sector_new$PresRT <- (as.numeric(cm_table_sector_new$RT_P)/(as.numeric(cm_table_sector_new$RT_P) + as.numeric(cm_table_sector_new$RT_BR1)))

#Calculate Sensitivity
Metric_country_new$SenLR <- (as.numeric(cm_table_country_new$LR_P)/(as.numeric(cm_table_country_new$LR_P) + as.numeric(cm_table_country_new$LR_BR2)))
Metric_country_new$SenNBR <- (as.numeric(cm_table_country_new$NBR_P)/(as.numeric(cm_table_country_new$NBR_P) + as.numeric(cm_table_country_new$NBR_BR2)))
Metric_country_new$SenRT <- (as.numeric(cm_table_country_new$RT_P)/(as.numeric(cm_table_country_new$RT_P) + as.numeric(cm_table_country_new$RT_BR2)))

Metric_sector_new$SenLR <- (as.numeric(cm_table_sector_new$LR_P)/(as.numeric(cm_table_sector_new$LR_P) + as.numeric(cm_table_sector_new$LR_BR2)))
Metric_sector_new$SenNBR <- (as.numeric(cm_table_sector_new$NBR_P)/(as.numeric(cm_table_sector_new$NBR_P) + as.numeric(cm_table_sector_new$NBR_BR2)))
Metric_sector_new$SenRT <- (as.numeric(cm_table_sector_new$RT_P)/(as.numeric(cm_table_sector_new$RT_P) + as.numeric(cm_table_sector_new$RT_BR2)))

Metric_country_new$PresLR[is.nan(Metric_country_new$PresLR)] <- 0

#Calculate F1 score
#f1_score <- 2 * (precision_score * sensitivity_score) / (precision_score + sensitivity_score)
Metric_country_new$F1LR <- 2 * (Metric_country_new$PresLR * Metric_country_new$SenLR) / (Metric_country_new$PresLR + Metric_country_new$SenLR)
Metric_country_new$F1NBR <- 2 * (Metric_country_new$PresNBR * Metric_country_new$SenNBR) / (Metric_country_new$PresNBR + Metric_country_new$SenNBR)
Metric_country_new$F1RT <- 2 * (Metric_country_new$PresRT * Metric_country_new$SenRT) / (Metric_country_new$PresRT + Metric_country_new$SenRT)

Metric_sector_new$F1LR <- 2 * (Metric_sector_new$PresLR * Metric_sector_new$SenLR) / (Metric_sector_new$PresLR + Metric_sector_new$SenLR)
Metric_sector_new$F1NBR <- 2 * (Metric_sector_new$PresNBR * Metric_sector_new$SenNBR) / (Metric_sector_new$PresNBR + Metric_sector_new$SenNBR)
Metric_sector_new$F1RT <- 2 * (Metric_sector_new$PresRT * Metric_sector_new$SenRT) / (Metric_sector_new$PresRT + Metric_sector_new$SenRT)

Metric_country_new$F1LR[is.nan(Metric_country_new$F1LR)] <- 0
Metric_country_new$F1RT[is.nan(Metric_country_new$F1RT)] <- 0

#Find Highest F1 score for each
#country
#Count
LR_count_c <- 0
NBR_count_c <- 0
RT_count_c <- 0

for (i in 1:nrow(Metric_country_new)) {
  Best <- max(Metric_country_new[i,8], Metric_country_new[i,9], Metric_country_new[i,10])
  if (Best == Metric_country_new[i,8]){
    Best <- "Logistic Regression"
    LR_count_c <- 1 + LR_count_c
  } else if (Best  == Metric_country_new[i,9]){
    Best <- "Negative Binomial Regression"
    NBR_count_c <- 1 + NBR_count_c
  } else {
    Best <- "Regression Tree"
    RT_count_c <- 1 + RT_count_c
  }
  Metric_country_new[i,11] <- Best
}

#sector
#Count
LR_count_s <- 0
NBR_count_s <- 0
RT_count_s <- 0

for (i in 1:nrow(Metric_sector_new)) {
  Best <- max(Metric_sector_new[i,8], Metric_sector_new[i,9], Metric_sector_new[i,10])
  if (Best == Metric_sector_new[i,8]){
    Best <- "Logistic Regression"
    LR_count_s <- 1 + LR_count_s
  } else if (Best  == Metric_sector_new[i,9]){
    Best <- "Negative Binomial Regression"
    NBR_count_s <- 1 + NBR_count_s
  } else {
    Best <- "Regression Tree"
    RT_count_s <- 1 + RT_count_s
  }
  Metric_sector_new[i,11] <- Best
}

#Summarized Table
metricstable_new <- data.frame(
  methods = c("Logistic Regression", "Negative Binomial Regression", "Regression Tree"),
  Country = c(LR_count_c,NBR_count_c,RT_count_c),
  Sector = c(LR_count_s,NBR_count_s,RT_count_s)
)
