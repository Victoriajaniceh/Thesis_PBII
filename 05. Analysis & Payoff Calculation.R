#Predictions table
#------
Predictions <- as.data.frame(1:4283)
Predictions$`1:4283` <- 1:4283
Predictions$DR <- df$DR

Predictions$LR1 <- predict_LR1
Predictions$LR2 <- predict_LR2

Predictions$NBR1 <- predict_NBR1
Predictions$NBR2 <- predict_NBR2

Predictions$RT1 <- predict_RT1
Predictions$RT2 <- predict_RT2

colnames(Predictions) <- c("No", "DR","LR1"," LR2", "NBR1", "NBR2", "RT1", "RT2")

write_xlsx(Predictions, 'Predictions New model 1.xlsx')
#Predictions <- read_xlsx("Predictions New model 1.xlsx")
#------

##Hitung MAE dan MSE
#------
MAE <- NA
tablemae <- data.frame(DR = Predictions$DR, Model = Predictions$No)
MAE_Table <- data.frame(Model = c("LR1", "LR2", "NBR1", "NBR2", "RT1", "RT2"), MAE = NA)

for (i in 1:6){
  tablemae$Model <- Predictions[,2+i]
  tablemae$abs_error <- abs(tablemae$DR - tablemae[,2])
  MAE <- sum(tablemae[,3])/3909
  MAE_Table[i,2] <- MAE
}

MSE <- NA
tablemse <- data.frame(DR = Predictions$DR, Model = Predictions$No)
MSE_Table <- data.frame(Model = c("LR1", "LR2", "NBR1", "NBR2", "RT1", "RT2"), MSE = NA)

for (i in 1:6){
  tablemse$Model <- Predictions[,2+i]
  tablemse$square_error <- (tablemse$DR - tablemse[,2])^2
  MSE <- sum(tablemse[,3])/3909
  MSE_Table[i,2] <- MSE
}

write_xlsx(MAE_Table, 'MAE Table.xlsx')
write_xlsx(MSE_Table, 'MSE Table.xlsx')
#------

##Calculate Payoff
#------
#Create All dataframes for each sector and country
#df <- read_xlsx("fulldatasetfix.xlsx") sblm pakek 
dataframe_payoff <- df[,c(1:4,6:7)]
dataframe_payoff$LR <- Predictions$LR1
dataframe_payoff$NBR <- Predictions$NBR1
dataframe_payoff$RT <- Predictions$RT1
dataframe <- as.data.frame(NA)
lists <- list()
fulllist<- list()

countries <- unique(df$Country)
sectors <- unique(df$Sector)
Premi <- 1
COR <- 0.5
Exp <- 0.15

for (i in 1:32){
  for (j in 1:12){
    dataframe <- dataframe_payoff[dataframe_payoff$Country == countries[i] & dataframe_payoff$Sector == sectors[j], ]
    lists[[j]] <- dataframe
    lists[[j]]$Y_bar <- c(0,cumsum(lists[[j]]$DR)/seq(1:nrow(lists[[j]])))[-(nrow(lists[[j]])+1)]
    lists[[j]]$K_LR <- c(0,cumsum(lists[[j]]$LR)/seq(1:nrow(lists[[j]])))[-(nrow(lists[[j]])+1)]
    lists[[j]]$K_NBR <- c(0,cumsum(lists[[j]]$NBR)/seq(1:nrow(lists[[j]])))[-(nrow(lists[[j]])+1)]
    lists[[j]]$K_RT <- c(0,cumsum(lists[[j]]$RT)/seq(1:nrow(lists[[j]])))[-(nrow(lists[[j]])+1)]
    lists[[j]]$y_Y_bar <- lists[[j]]$DR - lists[[j]]$Y_bar
    lists[[j]]$y_K_LR <- lists[[j]]$LR - lists[[j]]$Y_bar
    lists[[j]]$y_K_NBR <- lists[[j]]$NBR - lists[[j]]$Y_bar
    lists[[j]]$y_K_RT <- lists[[j]]$RT - lists[[j]]$Y_bar
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
saveWorkbook(wb, "Tabel Payoff Negara dan Sektor Fix Model 1.xlsx", overwrite = FALSE)

#Buat df masing-masing
for (i in seq(fulllist)){
  assign(paste0("df_", fulllist[[i]]$Country[1], "_", fulllist[[i]]$Sector[1]), fulllist[[i]])
}
#------

#Confusion Matrix for Basis Risk (check best regression method for index insurance)
#-----
conf_matrix <- data.frame(Country = NA,Sector = NA, Year = NA, PM_bin =NA ,PI_LR_bin = NA, PI_NBR_bin= NA, PI_RT_bin = NA)

# Confusion Matrix for all data
for (i in (seq(fulllist))){
  cm <- fulllist[[i]][,c(1:3,22:25)]
  conf_matrix <- rbind(conf_matrix,cm)
}

conf_matrix <- na.omit(conf_matrix)

#Each country and sector
# create a list to store the confusion matrices
confusion_matrices_country <- list()

# loop through each unique combination ofcountry
for (c in unique(conf_matrix$Country)) {
    # subset the data frame by country
    subset_df <- conf_matrix[conf_matrix$Country == c,]
    
    # create a confusion matrix for var1 and var2
    confusion_matrix_LR <- table(factor(subset_df$PM_bin, levels = c(0, 1)), factor(subset_df$PI_LR_bin, levels = c(0, 1)))
    confusion_matrix_NBR <- table(actual = subset_df$PM_bin, predicted = subset_df$PI_NBR_bin)
    confusion_matrix_RT <- table(factor(subset_df$PM_bin, levels = c(0, 1)), factor(subset_df$PI_RT_bin, levels = c(0, 1)))
    
    # add the confusion matrix to the list
    confusion_matrices_country[[c]]$LR_BR1 <- confusion_matrix_LR[1,2]
    confusion_matrices_country[[c]]$LR_BR2 <- confusion_matrix_LR[2,1]
    confusion_matrices_country[[c]]$LR_P <- confusion_matrix_LR[2,2]
    confusion_matrices_country[[c]]$LR_TP <- confusion_matrix_LR[1,1]
    confusion_matrices_country[[c]]$NBR_BR1 <- confusion_matrix_NBR[1,2]
    confusion_matrices_country[[c]]$NBR_BR2 <- confusion_matrix_NBR[2,1]
    confusion_matrices_country[[c]]$NBR_P <- confusion_matrix_NBR[2,2]
    confusion_matrices_country[[c]]$NBR_TP <- confusion_matrix_NBR[1,1]
    confusion_matrices_country[[c]]$RT_BR1 <- confusion_matrix_RT[1,2]
    confusion_matrices_country[[c]]$RT_BR2 <- confusion_matrix_RT[2,1]
    confusion_matrices_country[[c]]$RT_P <- confusion_matrix_RT[2,2]
    confusion_matrices_country[[c]]$RT_TP <- confusion_matrix_RT[1,1]
}

#Count
LR_count_c <- 0
NBR_count_c <- 0
RT_count_c <- 0

# find best method for each country
for (i in seq(confusion_matrices_country)) {
  LR_Error <- confusion_matrices_country[[i]]$LR_BR1 + confusion_matrices_country[[i]]$LR_BR2
  NBR_Error <- confusion_matrices_country[[i]]$NBR_BR1 + confusion_matrices_country[[i]]$NBR_BR2
  RT_Error <- confusion_matrices_country[[i]]$RT_BR1 + confusion_matrices_country[[i]]$RT_BR2
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
  confusion_matrices_country[[i]]$Best_Method <- Best
}

# create a list to store the confusion matrices
confusion_matrices_sector <- list()

# loop through each unique combination of sector
for (s in unique(conf_matrix$Sector)) {
  # subset the data frame by sector
  subset_df <- conf_matrix[conf_matrix$Sector == s,]
  
  # create a confusion matrix for var1 and var2
  confusion_matrix_LR <- table(subset_df$PM_bin, subset_df$PI_LR_bin)
  confusion_matrix_NBR <- table(subset_df$PM_bin, subset_df$PI_NBR_bin)
  confusion_matrix_RT <- table(subset_df$PM_bin, subset_df$PI_RT_bin)
  
  # add the confusion matrix to the list
  # add the confusion matrix to the list
  confusion_matrices_sector[[s]]$LR_BR1 <- confusion_matrix_LR[1,2]
  confusion_matrices_sector[[s]]$LR_BR2 <- confusion_matrix_LR[2,1]
  confusion_matrices_sector[[s]]$LR_P <- confusion_matrix_LR[2,2]
  confusion_matrices_sector[[s]]$LR_TP <- confusion_matrix_LR[1,1]
  confusion_matrices_sector[[s]]$NBR_BR1 <- confusion_matrix_NBR[1,2]
  confusion_matrices_sector[[s]]$NBR_BR2 <- confusion_matrix_NBR[2,1]
  confusion_matrices_sector[[s]]$NBR_P <- confusion_matrix_NBR[2,2]
  confusion_matrices_sector[[s]]$NBR_TP <- confusion_matrix_NBR[1,1]
  confusion_matrices_sector[[s]]$RT_BR1 <- confusion_matrix_RT[1,2]
  confusion_matrices_sector[[s]]$RT_BR2 <- confusion_matrix_RT[2,1]
  confusion_matrices_sector[[s]]$RT_P <- confusion_matrix_RT[2,2]
  confusion_matrices_sector[[s]]$RT_TP <- confusion_matrix_RT[1,1]
}

#Count
LR_count_s <- 0
NBR_count_s <- 0
RT_count_s <- 0

# find best method for each sector
for (i in seq(confusion_matrices_sector)) {
  LR_Error <- confusion_matrices_sector[[i]]$LR_BR1 + confusion_matrices_sector[[i]]$LR_BR2
  NBR_Error <- confusion_matrices_sector[[i]]$NBR_BR1 + confusion_matrices_sector[[i]]$NBR_BR2
  RT_Error <- confusion_matrices_sector[[i]]$RT_BR1 + confusion_matrices_sector[[i]]$RT_BR2
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
  confusion_matrices_sector[[i]]$Best_Method <- Best
}

cm_table_country <- do.call(rbind, confusion_matrices_country)
cm_table_country <- cbind(Country = rownames(cm_table_country), cm_table_country)
cm_table_country <- as.data.frame(cm_table_country)

cm_table_sector <- do.call(rbind, confusion_matrices_sector)
cm_table_sector <- cbind(Sector = rownames(cm_table_sector), cm_table_sector)
cm_table_sector <- as.data.frame(cm_table_sector)

#Summarized Table
methodstable <- data.frame(
  methods = c("Logistic Regression", "Negative Binomial Regression", "Regression Tree"),
  Country = c(LR_count_c,NBR_count_c,RT_count_c),
  Sector = c(LR_count_s,NBR_count_s,RT_count_s)
)
#-----

#Calculate F1 Score
#----
Metric_country <- data.frame(Country = 1:31, PresLR = NA, PresNBR = NA, PresRT = NA, SenLR = NA, SenNBR = NA, SenRT = NA, F1LR = NA, F1NBR = NA, F1RT = NA, Best_Method = NA)
Metric_sector <- data.frame(Sector = 1:12, PresLR = NA, PresNBR = NA, PresRT = NA, SenLR = NA, SenNBR = NA, SenRT = NA, F1LR = NA, F1NBR = NA, F1RT = NA, Best_Method = NA)

Metric_country$Country <- cm_table_country$Country
Metric_sector$Sector <- cm_table_sector$Sector

#Calculate Precision
Metric_country$PresLR <- (as.numeric(cm_table_country$LR_P)/(as.numeric(cm_table_country$LR_P) + as.numeric(cm_table_country$LR_BR1)))
Metric_country$PresNBR <- (as.numeric(cm_table_country$NBR_P)/(as.numeric(cm_table_country$NBR_P) + as.numeric(cm_table_country$NBR_BR1)))
Metric_country$PresRT <- (as.numeric(cm_table_country$RT_P)/(as.numeric(cm_table_country$RT_P) + as.numeric(cm_table_country$RT_BR1)))

Metric_sector$PresLR <- (as.numeric(cm_table_sector$LR_P)/(as.numeric(cm_table_sector$LR_P) + as.numeric(cm_table_sector$LR_BR1)))
Metric_sector$PresNBR <- (as.numeric(cm_table_sector$NBR_P)/(as.numeric(cm_table_sector$NBR_P) + as.numeric(cm_table_sector$NBR_BR1)))
Metric_sector$PresRT <- (as.numeric(cm_table_sector$RT_P)/(as.numeric(cm_table_sector$RT_P) + as.numeric(cm_table_sector$RT_BR1)))

#Calculate Sensitivity
Metric_country$SenLR <- (as.numeric(cm_table_country$LR_P)/(as.numeric(cm_table_country$LR_P) + as.numeric(cm_table_country$LR_BR2)))
Metric_country$SenNBR <- (as.numeric(cm_table_country$NBR_P)/(as.numeric(cm_table_country$NBR_P) + as.numeric(cm_table_country$NBR_BR2)))
Metric_country$SenRT <- (as.numeric(cm_table_country$RT_P)/(as.numeric(cm_table_country$RT_P) + as.numeric(cm_table_country$RT_BR2)))

Metric_sector$SenLR <- (as.numeric(cm_table_sector$LR_P)/(as.numeric(cm_table_sector$LR_P) + as.numeric(cm_table_sector$LR_BR2)))
Metric_sector$SenNBR <- (as.numeric(cm_table_sector$NBR_P)/(as.numeric(cm_table_sector$NBR_P) + as.numeric(cm_table_sector$NBR_BR2)))
Metric_sector$SenRT <- (as.numeric(cm_table_sector$RT_P)/(as.numeric(cm_table_sector$RT_P) + as.numeric(cm_table_sector$RT_BR2)))

Metric_country$PresLR[is.nan(Metric_country$PresLR)] <- 0

#Calculate F1 score
#f1_score <- 2 * (precision_score * sensitivity_score) / (precision_score + sensitivity_score)
Metric_country$F1LR <- 2 * (Metric_country$PresLR * Metric_country$SenLR) / (Metric_country$PresLR + Metric_country$SenLR)
Metric_country$F1NBR <- 2 * (Metric_country$PresNBR * Metric_country$SenNBR) / (Metric_country$PresNBR + Metric_country$SenNBR)
Metric_country$F1RT <- 2 * (Metric_country$PresRT * Metric_country$SenRT) / (Metric_country$PresRT + Metric_country$SenRT)

Metric_sector$F1LR <- 2 * (Metric_sector$PresLR * Metric_sector$SenLR) / (Metric_sector$PresLR + Metric_sector$SenLR)
Metric_sector$F1NBR <- 2 * (Metric_sector$PresNBR * Metric_sector$SenNBR) / (Metric_sector$PresNBR + Metric_sector$SenNBR)
Metric_sector$F1RT <- 2 * (Metric_sector$PresRT * Metric_sector$SenRT) / (Metric_sector$PresRT + Metric_sector$SenRT)

Metric_country$F1LR[is.nan(Metric_country$F1LR)] <- 0
Metric_country$F1RT[is.nan(Metric_country$F1RT)] <- 0

#Find Highest F1 score for each
#country
#Count
LR_count_c <- 0
NBR_count_c <- 0
RT_count_c <- 0

for (i in 1:nrow(Metric_country)) {
  Best <- max(Metric_country[i,8], Metric_country[i,9], Metric_country[i,10])
  if (Best == Metric_country[i,8]){
    Best <- "Logistic Regression"
    LR_count_c <- 1 + LR_count_c
  } else if (Best  == Metric_country[i,9]){
    Best <- "Negative Binomial Regression"
    NBR_count_c <- 1 + NBR_count_c
  } else {
    Best <- "Regression Tree"
    RT_count_c <- 1 + RT_count_c
  }
  Metric_country[i,11] <- Best
}

#sector
#Count
LR_count_s <- 0
NBR_count_s <- 0
RT_count_s <- 0

for (i in 1:nrow(Metric_sector)) {
  Best <- max(Metric_sector[i,8], Metric_sector[i,9], Metric_sector[i,10])
  if (Best == Metric_sector[i,8]){
    Best <- "Logistic Regression"
    LR_count_s <- 1 + LR_count_s
  } else if (Best  == Metric_sector[i,9]){
    Best <- "Negative Binomial Regression"
    NBR_count_s <- 1 + NBR_count_s
  } else {
    Best <- "Regression Tree"
    RT_count_s <- 1 + RT_count_s
  }
  Metric_sector[i,11] <- Best
}

#Summarized Table
metricstable <- data.frame(
  methods = c("Logistic Regression", "Negative Binomial Regression", "Regression Tree"),
  Country = c(LR_count_c,NBR_count_c,RT_count_c),
  Sector = c(LR_count_s,NBR_count_s,RT_count_s)
)
