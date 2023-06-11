#Regression Tree Model

install.packages("rpart") #Regression Tree
install.packages("rpart.plot") #Regression Tree Plot

library(dplyr)
library(readxl)
library(rpart) #Regression Tree
library(rpart.plot) #Regression Tree Plot

#Input Data
#-----
setwd("/Users/victoriajanice/Documents/U N I/Thesis/Dataset")
df_RT <- read_xlsx("fulldatasetfixnew.xlsx")
df_RT <- df_RT[ -c(6:7) ] #drop Pop and ND column
#-----

#Data Split
#-----
data_train_RT <- subset(df_RT,Year >= "2005" & Year <= "2017") #2005-2017
data_test_RT <- subset(df_RT,Year >= "2018" & Year < "2021") 

#Ori data train 2005-2017 data test 2018-2019
#ukdataset <- fulldataset_new[(fulldataset_new$Country=="United Kingdom"),] cek yg 2020 trnyt ga ada

##Drop not needed columns
data_train_RT <- data_train_RT[-3] #drop year
data_test_RT1 <- data_test_RT[-c(3)] #drop year
data_test_RT2 <- data_test_RT[-c(1,3)] #Country and drop year

#checkhasil <- data_test_RT$DR

#Data for predictions -> for calculating payoff
fulldatatest_RT <- df_RT[-c(3:4)]
#-----

#Model Regression Tree
#-----
#Dengan negara
rt_model1 <- rpart(DR ~ Country + Sector + BR + Prod + GDP + UR + CPG + IR + D, data=data_train_RT, method = "anova")
summary(rt_model1)
rpart.plot(rt_model1, type = 1, digits = 3, fallen.leaves = TRUE, cex = 0.7, compress = TRUE, ycompress = TRUE, faclen = -3)
print(rt_model1, minlength = 0, spaces = 2, digits = getOption("digits"),
      nsmall = min(20, digits),)

#Predict with full dataset
predict_RT1 <- predict(rt_model1, fulldatatest_RT)
head(predict_RT1)

rt_model1$variable.importance

var_imp_1 <- data.frame(
  Variable = names(rt_model1$variable.importance),
  Importance = round(rt_model1$variable.importance,5)
)

var_imp_1 %>%
  mutate(Variabel = factor(Variable, 
    levels = Variable[order(var_imp_1$Importance)])) %>%
  ggplot(aes(x = Variabel, y = Importance)) + 
  geom_col() +
  coord_flip() +
  geom_text(aes(label = Importance), size = 3,hjust = +0.5) +
  ggtitle("Model dengan Variabel Negara") +
  theme(plot.title = element_text(hjust = 0.5))

#Tanpa negara
rt_model2 <- rpart(DR ~ Sector + BR + Prod + GDP + UR + CPG + IR + D, data=data_train_RT, method = "anova")
summary(rt_model2)
#rpart.plot(rt_model2, type = 3, digits = 3, fallen.leaves = TRUE)
rpart.plot(rt_model2, type = 1, digits = 4, fallen.leaves = TRUE, cex = 1, compress = TRUE, ycompress = TRUE, faclen = -3)
print(rt_model2, minlength = 0, spaces = 2, digits = getOption("digits"),
      nsmall = min(20, digits),)

#Predict with full dataset
predict_RT2 <- predict(rt_model2, fulldatatest_RT)
head(predict_RT2)

var_imp_2 <- data.frame(
  Variable = names(rt_model2$variable.importance),
  Importance = round(rt_model2$variable.importance,5)
)

var_imp_2 %>%
  mutate(Variabel = factor(Variable, 
                           levels = Variable[order(var_imp_2$Importance)])) %>%
  ggplot(aes(x = Variabel, y = Importance)) + 
  geom_col() +
  coord_flip() +
  geom_text(aes(label = Importance), size = 3,hjust = +0.5) +
  ggtitle("Model tanpa Variabel Negara") +
  theme(plot.title = element_text(hjust = 0.5))


#Calculate Error
#printcp(rt_model2)
# Explicitly request the lowest cp value
#rt_model2$cptable[which.min(rt_model2$cptable[,"xerror"]),"CP"] #xerror : cross-validated error rate
#-----

