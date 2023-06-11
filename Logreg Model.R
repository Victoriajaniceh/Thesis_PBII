#Logistic Regression Model

#install.packages('Hmisc') #Binning
install.packages("Information")
install.packages('scorecard')

library(readxl)
library(writexl)
library(dplyr)
library(Hmisc) #Binning
library(Information) #WOE
library(scorecard) #Optimal bins

#Input Data
#-----
setwd("/Users/victoriajanice/Documents/U N I/Thesis/Dataset")
df <- read_xlsx("fulldatasetfixnew.xlsx")
df <- df[ -c(6:7) ] #drop unused column, year masi ada
#-----

#data biner loop
#-----
#Calculate Threshold
binary <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(binary) <- c("Percentile", "y*")
for(i in 1:21){
  binary[i,1] <- (0.71-(0.01*i))*100
  binary[i,2] <- quantile(df$DR, probs = 0.71-(0.01*i)) 
}

#Make into binary
for(i in 1:21){
  df[,i+11] <- ifelse(df$DR > binary[22-i,2], 1, 0)
}
#Rename columns
names(df)[12:ncol(df)]<- paste0(rep("v", each=21), "_",50:70)
#-----

#Buat data biner (not for appendix)
#-----
##Untuk pexc = 0.1, maka y* = percentile ke 90
#y_star1 <- quantile(df$DR, probs = 0.90) #0.126
#df$v1 <- ifelse(df$DR > y_star1, 1, 0) #v = 1 apabila y > y* & v = 0 apabila y <= y*
#table(df$v1) #0:3592 & 1:389
#-----

#Cek Asumsi untuk model Regresi Logistik
#-----
#Assumption #1: Variabel target harus biner -> memang hasilnya prob namun untuk data training perlu data 0 1 
#Assumption #2: Pengamatan independen satu dengan yang lain.
#Assumption #3: Tidak ada multikolinearitas -> Cek pake VIF (Sebagai aturan praktis, nilai VIF yang melebihi 5 atau 10 menunjukkan jumlah multikolinearitas yang bermasalah.)
#Assumption #4: Hubungan linear antara variabel independen kontinu dan log-odds -> bisa di cek dengan visualisasi data -> Checked using binning
#Assumption #5: Membutuhkan ukuran sampel yang besar.

write_xlsx(df, 'LRdatasetnew.xlsx')

#table(df$v_70) #0: 2148 & 1: 2135
#-----

#Binning
#-----
par(mfrow = c(2, 4))
#Birth Rate
flag_BR <- as.data.frame(cut2(df$BR, g=6))
flag_BR$BR <- df$BR
flag_BR$v_70 <- df$v_70
colnames(flag_BR) <- c("Bins","BR", "v_70")
summary(flag_BR)
flag_BR <- flag_BR %>% group_by(Bins)
agg_tbl_BR <- flag_BR %>% summarise(mean(v_70))
plot(agg_tbl_BR, main = "Birth Rate")  #LINIER NAIK

#Prod Rate
flag_PR <- as.data.frame(cut2(df$Prod, g=6))
flag_PR$Prod <- df$Prod
flag_PR$v_70 <- df$v_70
colnames(flag_PR) <- c("Bins","Prod", "v_70")
grp_PR <- flag_PR %>% group_by(Bins)
agg_tbl_PR <- grp_PR %>% summarise(mean(v_70))
plot(agg_tbl_PR, main = "Productivity Rate") #LINIER TURUN

#GDP
flag_GDP <- as.data.frame(cut2(df$GDP, g=6))
summary(flag_GDP)
flag_GDP$GDP <- df$GDP
flag_GDP$v_70 <- df$v_70
colnames(flag_GDP) <- c("Bins","GDP", "v_70")
grp_GDP <- flag_GDP %>% group_by(Bins)
agg_tbl_GDP <- grp_GDP %>% summarise(mean(v_70))
plot(agg_tbl_GDP, main = "GDP")  #LINIER TURUN

#UR
flag_UR <- as.data.frame(cut2(df$UR, g=6))
flag_UR$UR <- df$UR
flag_UR$v_70 <- df$v_70
colnames(flag_UR) <- c("Bins","UR", "v_70")
grp_UR <- flag_UR %>% group_by(Bins)
agg_tbl_UR <- grp_UR %>% summarise(mean(v_70))
plot(agg_tbl_UR, main = "Unemployment Rate")  #LINIER NAIK

#CPG
flag_CPG <- as.data.frame(cut2(df$CPG, g=6))
flag_CPG$CPG <- df$CPG
flag_CPG$v_70 <- df$v_70
colnames(flag_CPG) <- c("Bins","CPG", "v_70")
grp_CPG <- flag_CPG %>% group_by(Bins)
agg_tbl_CPG <- grp_CPG %>% summarise(mean(v_70))
plot(agg_tbl_CPG, main = "Consumer Price Growth")  #LINIER NAIK

#IR
flag_IR <- as.data.frame(cut2(df$IR, g=6))
flag_IR$IR <- df$IR
flag_IR$v_70 <- df$v_70
colnames(flag_IR) <- c("Bins","IR", "v_70")
grp_IR <- flag_IR %>% group_by(Bins)
agg_tbl_IR <- grp_IR %>% summarise(mean(v_70))
plot(agg_tbl_IR, main = "Interest Rate") #LINIER TURUN

#D
flag_D <- as.data.frame(cut2(df$D, g=6))
flag_D$D <- df$D
flag_D$v_70 <- df$v_70
colnames(flag_D) <- c("Bins","D", "v_70")
grp_D <- flag_D %>% group_by(Bins)
agg_tbl_D <- grp_D %>% summarise(mean(v_70))
plot(agg_tbl_D, main = "Debt") #LINIER NAIK
#-----

#Looping Binning
#-----
## Masing-Masing Variable
#Birth Rate
pdf(file = "/Users/victoriajanice/Documents/U N I/Thesis/Dataset/BR Plot.pdf", width = 15, height = 7) 
flag_BR <- data.frame(1:nrow(df))
par(mfrow = c(3, 7))
for(i in 1:21){
  flag_BR <- as.data.frame(cut2(df$BR, g=5))
  flag_BR$BR <- df$BR
  flag_BR[,3] <- df[,i+11]
  colnames(flag_BR) <- c("Bins","BR", "v")
  flag_BR <- flag_BR %>% group_by(Bins)
  agg_tbl_BR <- flag_BR %>% summarise(mean(v))
  plot(agg_tbl_BR, main = substitute(paste('Birth Rate for', a), list(a=colnames(df)[i+11]))) 
}
dev.off()

#Prod Rate
pdf(file = "/Users/victoriajanice/Documents/U N I/Thesis/Dataset/PR Plot.pdf", width = 15, height = 7) 
flag_PR <- data.frame(1:nrow(df))
par(mfrow = c(3, 7))
for(i in 1:21){
  flag_PR <- as.data.frame(cut2(df$BR, g=5))
  flag_PR$Prod <- df$Prod
  flag_PR[,3] <- df[,i+11]
  colnames(flag_PR) <- c("Bins","BR", "v")
  flag_PR <- flag_PR %>% group_by(Bins)
  agg_tbl_PR <- flag_PR %>% summarise(mean(v))
  plot(agg_tbl_PR, main = substitute(paste('Productivity Rate for', a), list(a=colnames(df)[i+11]))) 
}
dev.off()

#GDP
pdf(file = "/Users/victoriajanice/Documents/U N I/Thesis/Dataset/GDP Plot.pdf", width = 15, height = 7) 
flag_GDP <- data.frame(1:nrow(df))
par(mfrow = c(3, 7))
for(i in 1:21){
  flag_GDP <- as.data.frame(cut2(df$GDP, g=5))
  flag_GDP$GDP <- df$GDP
  flag_GDP[,3] <- df[,i+11]
  colnames(flag_GDP) <- c("Bins","BR", "v")
  flag_GDP <- flag_GDP %>% group_by(Bins)
  agg_tbl_GDP <- flag_GDP %>% summarise(mean(v))
  plot(agg_tbl_GDP, main = substitute(paste('GDP Rate for', a), list(a=colnames(df)[i+11]))) 
}
dev.off()

#UR
pdf(file = "/Users/victoriajanice/Documents/U N I/Thesis/Dataset/UR Plot.pdf", width = 15, height = 7) 
flag_UR <- data.frame(1:nrow(df))
par(mfrow = c(3, 7))
for(i in 1:21){
  flag_UR <- as.data.frame(cut2(df$UR, g=5))
  flag_UR$UR <- df$UR
  flag_UR[,3] <- df[,i+11]
  colnames(flag_UR) <- c("Bins","BR", "v")
  flag_UR <- flag_UR %>% group_by(Bins)
  agg_tbl_UR <- flag_UR %>% summarise(mean(v))
  plot(agg_tbl_UR, main = substitute(paste('Unemployment Rate for', a), list(a=colnames(df)[i+11]))) 
}
dev.off()

#CPG
pdf(file = "/Users/victoriajanice/Documents/U N I/Thesis/Dataset/CPG Plot.pdf", width = 15, height = 7) 
flag_CPG <- data.frame(1:nrow(df))
par(mfrow = c(3, 7))
for(i in 1:21){
  flag_CPG <- as.data.frame(cut2(df$CPG, g=5))
  flag_CPG$CPG <- df$CPG
  flag_CPG[,3] <- df[,i+11]
  colnames(flag_CPG) <- c("Bins","BR", "v")
  flag_CPG <- flag_CPG %>% group_by(Bins)
  agg_tbl_CPG <- flag_CPG %>% summarise(mean(v))
  plot(agg_tbl_CPG, main = substitute(paste('CPG for', a), list(a=colnames(df)[i+11]))) 
}
dev.off()

#IR
pdf(file = "/Users/victoriajanice/Documents/U N I/Thesis/Dataset/IR Plot.pdf", width = 15, height = 7) 
flag_IR <- data.frame(1:nrow(df))
par(mfrow = c(3, 7))
for(i in 1:21){
  flag_IR <- as.data.frame(cut2(df$IR, g=5))
  flag_IR$IR <- df$IR
  flag_IR[,3] <- df[,i+11]
  colnames(flag_IR) <- c("Bins","BR", "v")
  flag_IR <- flag_IR %>% group_by(Bins)
  agg_tbl_IR <- flag_IR %>% summarise(mean(v))
  plot(agg_tbl_IR, main = substitute(paste('Interest Rate for', a), list(a=colnames(df)[i+11]))) 
}
dev.off()

#D
pdf(file = "/Users/victoriajanice/Documents/U N I/Thesis/Dataset/DR Plot.pdf", width = 15, height = 7) 
flag_D <- data.frame(1:nrow(df))
par(mfrow = c(3, 7))
for(i in 1:21){
  flag_D <- as.data.frame(cut2(df$D, g=5))
  flag_D$D <- df$D
  flag_D[,3] <- df[,i+11]
  colnames(flag_D) <- c("Bins","BR", "v")
  flag_D <- flag_D %>% group_by(Bins)
  agg_tbl_D <- flag_D %>% summarise(mean(v))
  plot(agg_tbl_D, main = substitute(paste('Debt for', a), list(a=colnames(df)[i+11]))) 
}
dev.off()

##Masing-Masing percentile
#Birth Rate
#bins <- data.frame(1:nrow(df))
#par(mfrow = c(1, 7))
#for(i in 1:colnames(df)){
#  bins <- as.data.frame(cut2(df$"i", g=5))
#  bins$BR <- df$BR
#  flag_BR[,3] <- df[,i+11]
#  colnames(flag_BR) <- c("Bins","BR", "v")
#  flag_BR <- flag_BR %>% group_by(Bins)
#  agg_tbl_BR <- flag_BR %>% summarise(mean(v))
#  plot(agg_tbl_BR, main = substitute(paste('Birth Rate for', a), list(a=colnames(df)[i+11]))) 
#}
#dev.off()
#-----

#Weight of Evidence transformation : GDP, CPG, IR, D for v_70
#-----
#Find Optimal Bins
#opt_bins1 <- woebin(df, y="v_60", x=c("GDP", "CPG", "IR", "D"))

#for (i in 1:4){
#  print(opt_bins1[[i]])
#}

par(mfrow = c(1, 4))

IV <- create_infotables(data=df, y="v_70", bins=10, parallel=FALSE)

#GDP
flag_GDP <- as.data.frame(cut2(df$GDP, g=10))
flag_GDP$v_70 <- df$v_70
colnames(flag_GDP) <- c("Bins", "v_70")
levels_GDP <- levels(flag_GDP$Bins)

IV_Table_GDP <- IV$Tables$GDP
IV_Table_GDP$GDP <- levels(flag_GDP$Bins)

for (i in levels_GDP){
  flag_GDP$WOE_GDP[flag_GDP$Bins == i] <- IV_Table_GDP$WOE[IV_Table_GDP$GDP == i]
}

grp_GDP <- flag_GDP %>% group_by(WOE_GDP)
agg_tbl_GDP <- grp_GDP %>% summarise(mean(v_70))
plot(agg_tbl_GDP, main = "GDP") 

#CPG
flag_CPG <- as.data.frame(cut2(df$CPG, g=10))
flag_CPG$v_70 <- df$v_70
colnames(flag_CPG) <- c("Bins", "v_70")
levels_CPG <- levels(flag_CPG$Bins)

IV_Table_CPG <- IV$Tables$CPG
IV_Table_CPG$CPG <- levels(flag_CPG$Bins)

for (i in levels_CPG){
  flag_CPG$WOE_CPG[flag_CPG$Bins == i] <- IV_Table_CPG$WOE[IV_Table_CPG$CPG == i]
}

grp_CPG <- flag_CPG %>% group_by(WOE_CPG)
agg_tbl_CPG <- grp_CPG %>% summarise(mean(v_70))
plot(agg_tbl_CPG, main = "CPG")  

#IR
flag_IR <- as.data.frame(cut2(df$IR, g=10))
flag_IR$v_70 <- df$v_70
colnames(flag_IR) <- c("Bins", "v_70")
levels_IR <- levels(flag_IR$Bins)

IV_4 <- create_infotables(data=df, y="v_70", bins=10, parallel=FALSE)
IV_Table_IR <- IV_4$Tables$IR
IV_Table_IR$IR <- levels(flag_IR$Bins)

for (i in levels_IR){
  flag_IR$WOE_IR[flag_IR$Bins == i] <- IV_Table_IR$WOE[IV_Table_IR$IR == i]
}

grp_IR <- flag_IR %>% group_by(WOE_IR)
agg_tbl_IR <- grp_IR %>% summarise(mean(v_70))
plot(agg_tbl_IR, main = "IR")  

#D
flag_D <- as.data.frame(cut2(df$D, g=10)) 
flag_D$v_70 <- df$v_70
colnames(flag_D) <- c("Bins", "v_70")
levels_D <- levels(flag_D$Bins)

IV_Table_D <- IV_4$Tables$D
IV_Table_D$D <- levels(flag_D$Bins)

for (i in levels_D){
  flag_D$WOE_D[flag_D$Bins == i] <- IV_Table_D$WOE[IV_Table_D$D == i]
}

grp_D <- flag_D %>% group_by(WOE_D)
agg_tbl_D <- grp_D %>% summarise(mean(v_70))
plot(agg_tbl_D, main = "D")  

#Input transformed variable
df$WOE_GDP <- flag_GDP$WOE_GDP
df$WOE_CPG <- flag_CPG$WOE_CPG
df$WOE_IR <- flag_IR$WOE_IR
df$WOE_D <- flag_D$WOE_D
#-----

#Data Split
#-----
##Check buat bantu nentuin split data
# group_by() on department
#grp_tbl <- df %>% group_by(Year)
#grp_tbl

# summarise on groupped data.
#agg_tbl <- grp_tbl %>% summarise(mean(DR))
#agg_tbl #2014 keatas mulai stabil

#plot(agg_tbl) #2014 keatas mulai stabil 

#nrow(subset(df,Year >= "2005" & Year <= "2017"))/nrow(df) #0.8550615
#nrow(subset(df,Year >= "2018" & Year < "2020"))/nrow(df) #0.1449385
#nrow(df)

#Data train 2005-2017
#Data test: 2018-2019

##Data Splitting
data_train <- subset(df,Year >= "2005" & Year <= "2017")
data_test <- subset(df,Year >= "2018" & Year < "2021")

##Drop not needed columns
data_train <- data_train[-c(3)]
data_test1 <- data_test[-c(3,12:32)] #Dengan Negara
data_test2 <- data_test[-c(1,3,12:32)] #Tanpa Negara

#Data for predictions -> for calculating payoff
fulldatatest_LR <- df[-c(3:4,12:32)]
#-----

#Model Logreg yg ga pake WOE
#-----
##Looping
#Dengan negara 
mod_summaries_1 <- list()  
pred_summ_1 <- data.frame(1:561)
Error_1 <- as.data.frame(binary$Percentile)

for(i in 1:21) {                 
  flags <- names(df)    
  mod_summaries_1[[i]] <- summary(glm(paste(flags[i+11], "~ Country + Sector + BR + Prod + GDP + UR + CPG + IR + D"), data = data_train, family = "binomial"))
  sig <- which(mod_summaries_1[[i]]$coefficients[c(43:49),4] < 0.05) #Smaller than alpha = 0.05
  sig_var <- as.character(names(sig))
  model <- glm(paste(flags[i+11], "~."), data = data_train[,c(paste(flags[i+11]), c("Country", "Sector", sig_var)) ], family = "binomial")
  mod_summaries_1[[i]] <- summary(model)
  predict <- predict(model, data_test1[,c("Country", "Sector", sig_var)], type = "response")
  predict <- ifelse(predict > binary[22-i,2], 1, 0)
  pred_summ_1 <- cbind(pred_summ_1,as.data.frame(predict))
  CF <- as.matrix(table(as.matrix(data_test[,paste(flags[i+11])]), predict))
  Error_1[22-i,2] <- (CF[1,2]+CF[2,1])/sum(CF)
}
names(pred_summ_1)[2:ncol(pred_summ_1)]<- paste0(rep("v", each=21), "_",50:70)
colnames(Error_1) <- c("Percentile", "Error")

Error_1[which(Error_1$Error == min(Error_1$Error)),] #Best Model is v_70

# Model final Dengan negara 
glm_model1 <- glm(v_70 ~ Country + Sector + BR + Prod + GDP + UR + CPG + D, data = data_train, family = "binomial")
summary(glm_model1)

#Predict with full dataset
predict_LR1 <- predict(glm_model1, fulldatatest_LR, type = "response")

#Tanpa negara
mod_summaries_2 <- list()  
pred_summ_2 <- data.frame(1:561)
Error_2 <- as.data.frame(binary$Percentile)

for(i in 1:21) {                 
  flags <- names(df)    
  mod_summaries_2[[i]] <- summary(glm(paste(flags[i+11], "~ Sector + BR + Prod + GDP + UR + CPG + IR + D"), data = data_train, family = "binomial"))
  sig <- which(mod_summaries_2[[i]]$coefficients[13:19,4] < 0.05) #Smaller than alpha = 0.05
  sig_var <- as.character(names(sig))
  model <- glm(paste(flags[i+11], "~."), data = data_train[,c(paste(flags[i+11]), c("Sector", sig_var)) ], family = "binomial")
  mod_summaries_2[[i]] <- summary(model)
  predict <- predict(model, data_test1[,c("Sector", sig_var)], type = "response")
  predict <- ifelse(predict > binary[22-i,2], 1, 0)
  pred_summ_2 <- cbind(pred_summ_2,as.data.frame(predict))
  CF <- as.matrix(table(as.matrix(data_test[,paste(flags[i+11])]), predict))
  Error_2[22-i,2] <- (CF[1,2]+CF[2,1])/sum(CF)
}
names(pred_summ_2)[2:ncol(pred_summ_2)]<- paste0(rep("v", each=21), "_",50:70)
colnames(Error_2) <- c("Percentile", "Error")

Error_2[which(Error_2$Error == min(Error_2$Error)),] #Best Model v_70

#Tanpa negara
glm_model2 <- glm(v_70 ~ Sector + BR + Prod + GDP + UR + CPG + D, data = data_train, family = "binomial")
summary(glm_model2)

#Predict with full dataset
predict_LR2 <- predict(glm_model2, fulldatatest_LR, type = "response")
#-----

#Model Logreg with WeightofEvidence
#-----
##Looping

flags <- names(df) 

#Dengan negara 
mod_summaries_1 <- list()  
pred_summ_1 <- data.frame(1:934)
Error_1_WOE <- as.data.frame(binary$Percentile)

for(i in 1:21) {  
  
  IV <- create_infotables(data=df, y=flags[i+11], bins=5, parallel=FALSE)
  
  #GDP
  flag_GDP <- as.data.frame(cut2(df$GDP, g=5))
  flag_GDP$binaryvar <- df[,i+11]
  colnames(flag_GDP) <- c("Bins", "binaryvar")
  levels_GDP <- levels(flag_GDP$Bins)
  
  IV_Table_GDP <- IV$Tables$GDP
  IV_Table_GDP$GDP <- levels(flag_GDP$Bins)
  
  for (k in levels_GDP){
    flag_GDP$WOE_GDP[flag_GDP$Bins == k] <- IV_Table_GDP$WOE[IV_Table_GDP$GDP == k]
  }
  
  #CPG
  flag_CPG <- as.data.frame(cut2(df$CPG, g=5))
  flag_CPG$binaryvar <- df[,i+11]
  colnames(flag_CPG) <- c("Bins", "binaryvar")
  levels_CPG <- levels(flag_CPG$Bins)
  
  IV_Table_CPG <- IV$Tables$CPG
  IV_Table_CPG$CPG <- levels(flag_CPG$Bins)
  
  for (k in levels_CPG){
    flag_CPG$WOE_CPG[flag_CPG$Bins == k] <- IV_Table_CPG$WOE[IV_Table_CPG$CPG == k]
  }
  
  #IR
  flag_IR <- as.data.frame(cut2(df$IR, g=5))
  flag_IR$binaryvar <- df[,i+11]
  colnames(flag_IR) <- c("Bins", "binaryvar")
  levels_IR <- levels(flag_IR$Bins)
  
  IV_Table_IR <- IV$Tables$IR
  IV_Table_IR$IR <- levels(flag_IR$Bins)
  
  for (k in levels_IR){
    flag_IR$WOE_IR[flag_IR$Bins == k] <- IV_Table_IR$WOE[IV_Table_IR$IR == k]
  }
  
  #D
  flag_D <- as.data.frame(cut2(df$D, g=5)) 
  flag_D$binaryvar <- df[,i+11]
  colnames(flag_D) <- c("Bins", "binaryvar")
  levels_D <- levels(flag_D$Bins)
  
  IV_Table_D <- IV$Tables$D
  IV_Table_D$D <- levels(flag_D$Bins)
  
  for (k in levels_D){
    flag_D$WOE_D[flag_D$Bins == k] <- IV_Table_D$WOE[IV_Table_D$D == k]
  }
  
  #Input transformed variable
  df$WOE_GDP <- flag_GDP$WOE_GDP
  df$WOE_CPG <- flag_CPG$WOE_CPG
  df$WOE_IR <- flag_IR$WOE_IR
  df$WOE_D <- flag_D$WOE_D
  
  ##Data Splitting
  data_train <- subset(df,Year >= "2005" & Year <= "2017")
  data_test <- subset(df,Year >= "2018" & Year < "2021")
  
  ##Drop not needed columns
  data_train <- data_train[-c(3)]
  data_test1 <- data_test[-c(3,12:32)] #Dengan Negara
  data_test2 <- data_test[-c(1,3,12:32)] #Tanpa Negara
  
  mod_summaries_1[[i]] <- summary(glm(paste(flags[i+11], "~ Country + Sector + BR + Prod + WOE_GDP + UR + WOE_CPG + WOE_IR + WOE_D"), data = data_train, family = "binomial"))
  sig <- which(mod_summaries_1[[i]]$coefficients[43:49,4] < 0.05) #Smaller than alpha = 0.05
  sig_var <- as.character(names(sig))
  model <- glm(paste(flags[i+11], "~."), data = data_train[,c(paste(flags[i+11]), c("Country", "Sector", sig_var)) ], family = "binomial")
  mod_summaries_1[[i]] <- summary(model)
  predict <- predict(model, data_test1[,c("Country", "Sector", sig_var)], type = "response")
  predict <- ifelse(predict > binary[22-i,2], 1, 0)
  pred_summ_1 <- cbind(pred_summ_1,as.data.frame(predict))
  #CF <- as.matrix(table(factor(as.matrix(data_test[,paste(flags[i+11])]), levels = c(0, 1)), factor(predict,levels = c(0, 1))))
  CF <- as.matrix(table(as.matrix(data_test[,paste(flags[i+11])]), predict))
  Error_1_WOE[22-i,2] <- (CF[1,2]+CF[2,1])/sum(CF)
}
names(pred_summ_1)[2:ncol(pred_summ_1)]<- paste0(rep("v", each=21), "_",50:70)
colnames(Error_1_WOE) <- c("Percentile", "Error")

Error_1_WOE[which(Error_1_WOE$Error == min(Error_1_WOE$Error)),] #Best Model is v_69
mod_summaries_1[[20]][["terms"]]

#Dengan negara 
glm_model1 <- glm(v_69 ~ Country + Sector + BR + UR + WOE_CPG + WOE_IR, data = data_train, family = "binomial")
summary(glm_model1)

#Predict with full dataset
predict_LR1 <- predict(glm_model1, fulldatatest_LR, type = "response")

#Tanpa negara
mod_summaries_2 <- list()  
pred_summ_2 <- data.frame(1:934)
Error_2_WOE <- as.data.frame(binary$Percentile)

for(i in 1:21) {                
  
  IV <- create_infotables(data=df, y=flags[i+11], bins=5, parallel=FALSE)
  
  #GDP
  flag_GDP <- as.data.frame(cut2(df$GDP, g=5))
  flag_GDP$binaryvar <- df[,i+11]
  colnames(flag_GDP) <- c("Bins", "binaryvar")
  levels_GDP <- levels(flag_GDP$Bins)
  
  IV_Table_GDP <- IV$Tables$GDP
  IV_Table_GDP$GDP <- levels(flag_GDP$Bins)
  
  for (k in levels_GDP){
    flag_GDP$WOE_GDP[flag_GDP$Bins == k] <- IV_Table_GDP$WOE[IV_Table_GDP$GDP == k]
  }
  
  #CPG
  flag_CPG <- as.data.frame(cut2(df$CPG, g=5))
  flag_CPG$binaryvar <- df[,i+11]
  colnames(flag_CPG) <- c("Bins", "binaryvar")
  levels_CPG <- levels(flag_CPG$Bins)
  
  IV_Table_CPG <- IV$Tables$CPG
  IV_Table_CPG$CPG <- levels(flag_CPG$Bins)
  
  for (k in levels_CPG){
    flag_CPG$WOE_CPG[flag_CPG$Bins == k] <- IV_Table_CPG$WOE[IV_Table_CPG$CPG == k]
  }
  
  #IR
  flag_IR <- as.data.frame(cut2(df$IR, g=5))
  flag_IR$binaryvar <- df[,i+11]
  colnames(flag_IR) <- c("Bins", "binaryvar")
  levels_IR <- levels(flag_IR$Bins)
  
  IV_Table_IR <- IV$Tables$IR
  IV_Table_IR$IR <- levels(flag_IR$Bins)
  
  for (k in levels_IR){
    flag_IR$WOE_IR[flag_IR$Bins == k] <- IV_Table_IR$WOE[IV_Table_IR$IR == k]
  }
  
  #D
  flag_D <- as.data.frame(cut2(df$D, g=5)) 
  flag_D$binaryvar <- df[,i+11]
  colnames(flag_D) <- c("Bins", "binaryvar")
  levels_D <- levels(flag_D$Bins)
  
  IV_Table_D <- IV$Tables$D
  IV_Table_D$D <- levels(flag_D$Bins)
  
  for (k in levels_D){
    flag_D$WOE_D[flag_D$Bins == k] <- IV_Table_D$WOE[IV_Table_D$D == k]
  }
  
  #Input transformed variable
  df$WOE_GDP <- flag_GDP$WOE_GDP
  df$WOE_CPG <- flag_CPG$WOE_CPG
  df$WOE_IR <- flag_IR$WOE_IR
  df$WOE_D <- flag_D$WOE_D
  
  ##Data Splitting
  data_train <- subset(df,Year >= "2005" & Year <= "2017")
  data_test <- subset(df,Year >= "2018" & Year < "2021")
  
  ##Drop not needed columns
  data_train <- data_train[-c(3)]
  data_test1 <- data_test[-c(3,12:32)] #Dengan Negara
  data_test2 <- data_test[-c(1,3,12:32)] #Tanpa Negara
  
  mod_summaries_2[[i]] <- summary(glm(paste(flags[i+11], "~ Sector + BR + Prod + WOE_GDP + UR + WOE_CPG + WOE_IR + WOE_D"), data = data_train, family = "binomial"))
  sig <- which(mod_summaries_2[[i]]$coefficients[13:19,4] < 0.05) #Smaller than alpha = 0.05
  sig_var <- as.character(names(sig))
  model <- glm(paste(flags[i+11], "~."), data = data_train[,c(paste(flags[i+11]), c("Sector", sig_var)) ], family = "binomial")
  mod_summaries_2[[i]] <- summary(model)
  predict <- predict(model, data_test2[,c("Sector", sig_var)], type = "response")
  predict <- ifelse(predict > binary[22-i,2], 1, 0)
  pred_summ_2 <- cbind(pred_summ_2,as.data.frame(predict))
  CF <- as.matrix(table(as.matrix(data_test[,paste(flags[i+11])]), predict))
  Error_2_WOE[22-i,2] <- (CF[1,2]+CF[2,1])/sum(CF)
}
names(pred_summ_2)[2:ncol(pred_summ_2)]<- paste0(rep("v", each=21), "_",50:70)
colnames(Error_2_WOE) <- c("Percentile", "Error")

Error_2_WOE[which(Error_2_WOE$Error == min(Error_2_WOE$Error)),] #Best Model v_69
mod_summaries_2[[21]][["terms"]]

#Tanpa negara
glm_model2 <- glm(v_70 ~ Sector + BR + WOE_GDP + UR + WOE_CPG + WOE_IR + WOE_D, data = data_train, family = "binomial")
summary(glm_model2)

#Predict with full dataset
predict_LR2 <- predict(glm_model2, fulldatatest_LR, type = "response")
#-----