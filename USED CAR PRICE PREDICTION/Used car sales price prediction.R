### BANA 288  Predictive Analytics 
### Team Project R.Script
### Team 9 Chen Zheng, C K Anand Prakash, Hui Xu, Jaewook Shin, Matthew Kwong
### Topic:  Used Car Sales Price Prediction

### Submission guide
### "This file does not need to be fully commented or completely working file.  This deliverable is many used to check that analysis was performed in R" on Canvas

### Work environment set up and Data Import

setwd("D:/Desktop/Learn Git/USED CAR PRICE PREDICTION")

data <- read.csv("dataset.csv")
library(ggplot2)
library(dplyr)
library(Metrics)
library(glmnet)

names(data)


### Data Cleaning

data[data$state=="ca",]

# Remove unnecessary variables

data$id <- NULL
data$X <- NULL

# convert binary variables to factor

data$transmission <- as.numeric(ifelse(data$transmission=="automatic", 1, 0))
data$awarded <- as.numeric(as.factor(data$awarded))

# Create new variables

data$Premium <- round(data$sales_price/data$mnf_price,4)

data$Age <- data$sale_yr - data$make_yr

data1 <- data[c(21,5,4,13,22,1,2,3,7:20)]

data1

names(data1)

# With dummy variables

# create model matrix for categorical variables

sale_yr <- as.factor(data1$sale_yr)
sale_mnt <- as.factor(data1$sale_mnt)
sale_day <- as.factor(data1$sale_day)
make <- as.factor(data1$make)
national <- as.factor(data1$national)
body <- as.factor(data1$body)
make_yr <- as.factor(data1$make_yr)
ext_col <- as.factor(data1$ext_col)
int_col <- as.factor(data1$int_col)
state <- as.factor(data1$state)

tmp_sale_yr <- data.frame(model.matrix(~sale_yr - 1))
tmp_sale_mnt <- data.frame(model.matrix(~sale_mnt - 1))
tmp_sale_day <- data.frame(model.matrix(~sale_day - 1))
tmp_make <- data.frame(model.matrix(~make - 1))
tmp_national <- data.frame(model.matrix(~national - 1))
tmp_body <- data.frame(model.matrix(~body - 1))
tmp_make_yr <- data.frame(model.matrix(~make_yr - 1))
tmp_ext_col <- data.frame(model.matrix(~ext_col - 1))
tmp_int_col <- data.frame(model.matrix(~int_col - 1))
tmp_state <- data.frame(model.matrix(~state - 1))

# Combine all together

data2 <- cbind(data[,c(21,1,2,3,4,5,11,13,14,15,16,20)], tmp_sale_yr, tmp_sale_mnt, tmp_sale_day, tmp_make, tmp_national, tmp_body, tmp_make_yr, tmp_ext_col, tmp_int_col, tmp_state)

# Remove string variables

data2$seller <- NULL
data2$sale_yr <- NULL
data2$sale_mnt <- NULL
data2$sale_day <- NULL
data2$int_col <- NULL
data2$X <- NULL
data2$id <- NULL
data2$body <- NULL
data2$model <- NULL
data2$make <- NULL
data2$trim <- NULL
data2$national <- NULL
data2$ext_col <- NULL
data2$state <- NULL

str(data1)
str(data2)

write.csv(data1, "data1.csv")
write.csv(data2, "data2.csv")

sum(is.na(data2))


### For introduction - Industry revenue trend of the US used car market

library(fpp3)

Yr <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
Rev <- c(5.96, 6.49, 7.14, 7.92, 8.83, 9.87, 10.37, 10.78, 9.81, 10.27)

a <- data.frame(Yr, Rev)

ggplot(a, aes(x=Yr, y=Rev)) + geom_col(width=0.4, fill=c("gray","gray","gray","gray","gray","gray","gray","gray","black", "black")) + labs(x="Year", y="Market Revenue($B)") + theme(panel.background = element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + geom_text(aes(label = round(Rev, 1)), vjust = -0.5) + scale_x_continuous(breaks = unique(a$Yr), labels = unique(a$Yr))


### Exploratory Data Analysis - Visualization

library(readr)
data1_ca <- read_csv("data1.ca.csv")
View(data1_ca)

str(data1_ca)
head(data1_ca)
library(ggplot2)

#scatter plot
#remove outliers in odometer
data1_ca_clean <-subset(data1_ca, odometer != 999999)
#create plot
options(scipen=999)
ggp<-ggplot(data1_ca_clean, aes(x = odometer, y = sales_price)) + 
  geom_point(size=0.5, show.legend = FALSE,alpha = 0.4) +
  theme_minimal
ggp
# plot a regression line
ggp+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")
model<-lm(sales_price ~ odometer, data1_ca_clean)
coef(model)
###  (Intercept)      odometer 
### 22650.0778019    -0.1209557 

#box plot
data1_ca$awarded <- as.factor(data1_ca$awarded)
data1_ca$awarded <- as.numeric(data1_ca$awarded) - 1

dat<-cbind(data1_ca$awarded, data1_ca)
dat

ggplot(dat, aes(x = as.factor(data1_ca$awarded), y = sales_price)) +
  geom_boxplot(fill = "#0099f8") +
  coord_flip()+
  theme_classic()+
  labs(x="awarded")

#condition
ggplot(dat, aes(x=condition, y=sales_price))+
  geom_point()

#create correlation matrix of (rounded to 2 decimal places)
dat1<-round(cor(dat[c('sale_yr','mnf_price','sales_price','awarded','make_yr','odometer','condition')]),2)

#visualize correlations
library(corrplot)
corrplot(dat1, method="circle")

a <- read.csv("real_final1.csv")
a <- a[-22]
a

a %>% group_by(national) %>% summarize(test=mean(Premium))
a %>% group_by(national) %>% summarize(test=mean(sales_price))

names(a)

a %>% group_by(body) %>% summarize(test=mean(Premium)) %>% arrange(desc(test))
a %>% group_by(body) %>% summarize(test=mean(sales_price)) %>% arrange(desc(test))

a %>% group_by(make) %>% summarize(test=mean(Premium)) %>% arrange(desc(test)) 
a %>% group_by(make) %>% summarize(test=mean(sales_price)) %>% arrange(desc(test))

a %>% group_by(model) %>% summarize(test=mean(Premium)) %>% arrange(desc(test)) 
a %>% group_by(model) %>% summarize(test=mean(sales_price)) %>% arrange(desc(test))

nrow(a)

a$awarded

a %>% group_by(awarded) %>% summarize(test=mean(sales_price))

plot(a$odometer, a$sales_price)

b <- read.csv("real_final2.csv")

sort(abs(cor(b)[1,]), decreasing=T)

a %>% group_by(national) %>% summarize(test=mean(odometer))

a1 <- a[a$odometer!=max(a$odometer),]

plot(a1$odometer, a1$sales_price)

plot(a1$condition, a1$sales_price)

a1 %>% group_by(sale_yr) %>% summarize(test=mean(Premium))

test <- table(a1$sale_yr)

test <- data.frame(test)

test

ggplot(test, aes(x=Var1, y=Freq)) + geom_col(fill=c("gray", "black")) + labs(x="", y="")+theme(panel.background = element_blank(), axis.ticks.y=element_line(), axis.text.y=element_blank())

a %>% group_by(national) %>% summarize(test=mean(condition))

boxplot(a$awarded, a$sales_price)

boxplot(a$transmission, a$sales_price)

plot(a$odometer, a$sales_price)

hist(a$Age)

plot(a$Age, a$Premium)

a %>% group_by(transmission) %>% summarize(test=mean(Premium))
a %>% group_by(awarded) %>% summarize(test=mean(Premium))

a %>% group_by(transmission) %>% summarize(test=mean(sales_price))
a %>% group_by(awarded) %>% summarize(test=mean(sales_price))

x <- a[,c("national", "make", "body", "sales_price", "Premium")]

x %>% group_by(national) %>% summarize(sal=mean(sales_price), pre=mean(Premium)) %>% arrange(sal, decreasin=T)


### Clustering

cardf1 <- read.csv("data1.ca.csv") # no dummys
cardf2 <- read.csv("data2.ca.csv") # with dummys
colnames(cardf1)[4] <- "MMR_price"
colnames(cardf2)[4] <- "MMR_price"
cardf1$awarded <- cardf1$awarded -1
cardf2$awarded <- cardf2$awarded -1

library(cluster)
library(NbClust)
library(factoextra)

# Create DF just cluster variables
clustervars3 <- c("Age", "odometer", "condition", "MMR_price")
clusterdf3 <- na.omit(cardf2[,clustervars3])
str(clusterdf3)

#standardize
clusterdf3.std <- scale(clusterdf3, center=TRUE, scale=TRUE)
round(apply(clusterdf3.std,2,mean),4) # all means are now 0 for each variable; 4 decimals
round(apply(clusterdf3.std,2,sd),4) # all standard deviations are now 1; 4 decimals

clusterdf3.1std <-cbind(clusterdf3.std,cardf2$awarded,cardf2$transmission)

colnames(clusterdf3.1std)[5:6] <- c("awarded", "transmission")

#Elbow plot
fviz_nbclust(x=clusterdf3.1std, FUNcluster = kmeans, nstart=100, method="wss", k.max = 10) + 
  labs(title="Optimal Number of Clusters: Elbow Plot") + 
  coord_cartesian(ylim=c(0,80000)) + geom_line(size=2) # ---> looks like K=2 optimal

# Start of clustering
set.seed(123456)
km3 <- kmeans(clusterdf3.1std, centers=2)

# Means of variables chosen for clustering
cluster_assignments3 <- km3$cluster
cluster_assignments3
cluster_means3 <- aggregate(clusterdf3.1std, by = list(cluster_assignments3), mean)
cluster_means3

# > cluster_means3
# Group.1        Age   odometer  condition    awarded transmission
# 1       1 -0.5721509 -0.5406504  0.4069312 0.05285459    0.9709337
# 2       2  1.1754084  1.1106948 -0.8359863 0.04856445    0.9561698

# Means for all of the variables if we're interested
cluster_alldata_means3.1 <- aggregate(cardf1, by = list(cluster_assignments3), mean)
cluster_alldata_means3.1
cluster_alldata_means3.2 <- aggregate(cardf2, by = list(cluster_assignments3), mean)
cluster_alldata_means3.2

# Adding cluster assignments to cardf
cluster_assignments_df3 <- as.data.frame(km3$cluster)
cardf_clustered3.1 <- cbind(cardf1,cluster_assignments_df3)
cardf_clustered3.2 <- cbind(cardf2,cluster_assignments_df3)

colnames(cardf_clustered3.1)[23] <- "ClusterNo"
colnames(cardf_clustered3.2)[148] <- "ClusterNo"

# Create a CSV that includes cluster assignments
write.csv(cardf_clustered3.1, "real_final1.csv", row.names=FALSE)
write.csv(cardf_clustered3.2, "real_final2.csv", row.names=FALSE)

# df's for each cluster 1 and 2
cardf1_c1 <- cardf_clustered3.1[cardf_clustered3.2$ClusterNo==1, ] # 13452
cardf1_c2 <- cardf_clustered3.1[cardf_clustered3.2$ClusterNo==2, ] # 6548

cardf2_c1 <- cardf_clustered3.2[cardf_clustered3.2$ClusterNo==1, ] # 13452
cardf2_c2 <- cardf_clustered3.2[cardf_clustered3.2$ClusterNo==2, ] # 6548


### Model Building(1) - Linear Regression

# Read data
data1 <- read.csv("real_final2.csv", header = TRUE)
names(data1)

#For removing NA for data set
data1 <- subset(data1, select = -c(Age,sale_daySun, makeIsuzu  , makeOldsmobile, make_yr1992, make_yr1993,make_yr1995,ext_collime,
                                   ext_colpink,ext_colturquoise, int_colburgundy, int_colgold,int_colgreen,int_colyellow
                                   , sales_price, make_yr) )
M <- cor(data1)
M
M[M > -0.8 ] <- ""
M

# Split data into clusters based on ClusterNo variable
cluster1 <- data1[data1$ClusterNo == 1, ]
cluster2 <- data1[data1$ClusterNo == 2, ]

#For removing NA from cluster 1
cluster1 <- subset(cluster1, select = -c(makePlymouth, makeSaab, makeSuzuki, makeVolvo, nationalSWE, nationalJPN, 
                                         nationalENG, nationalGER, nationalKOR,nationalITA, nationalUSA,
                                         sale_yr2014,sale_yr2015, sale_mntMay, sale_dayMon, sale_dayWed,
                                         make_yr1994, make_yr1996, make_yr1997, make_yr1998,make_yr1999,
                                         make_yr2000, make_yr2001, make_yr2002,
                                         make_yr2003, make_yr2005, make_yr2014, make_yr2015,ext_coloff.white,
                                         ext_colyellow, int_colblue , int_colwhite, bodyWagon,ClusterNo))

# Load the car packagelibrary(car)
library(car)

# Fit a linear regression model
cluster1_M1 <- lm(Premium ~ . , data = cluster1)
summary(cluster1_M1)

# Calculate VIF
vif_values <- vif(cluster1_M1)

# Set threshold for high VIF
vif_threshold <- 10

# Identify variables with VIF greater than the threshold
high_vif_vars <- names(vif_values[vif_values > vif_threshold])

# Print variables with high VIF
print(high_vif_vars)

# Remove variables with high VIF from the data
cluster1_filtered <- cluster1[, !names(cluster1) %in% high_vif_vars]
names(cluster1_filtered)


###########################################################################################
#For removing NA from cluster 2
cluster2 <- subset(cluster2, select = -c(sale_yr2014,sale_yr2015,sale_mntMay, sale_dayMon,
                                         sale_dayWed, makeAston.Martin,makeBentley, makeMaserati, makesmart,
                                         makeTesla, makeVolvo, nationalENG, nationalGER, nationalKOR,nationalITA, nationalUSA
                                         ,nationalJPN,bodyWagon, make_yr2013, make_yr2014, make_yr2015, ext_colyellow,nationalSWE
                                         ,int_coloff.white,int_colorange,int_colpurple, int_colwhite,int_colblue, ClusterNo))
# Fit a linear regression model
cluster2_M1 <- lm(Premium ~ . , data = cluster2)
summary(cluster2_M1)

# Calculate VIF
vif_values1 <- vif(cluster2_M1)

# Set threshold for high VIF
vif_threshold1 <- 10

# Identify variables with VIF greater than the threshold
high_vif_vars1 <- names(vif_values1[vif_values1 > vif_threshold1])

# Print variables with high VIF
print(high_vif_vars1)

# Remove variables with high VIF from the data
cluster2_filtered <- cluster2[, !names(cluster2) %in% high_vif_vars1]
names(cluster2_filtered)

#######################################################################################
#---------------------------------------
# Cluster 1 
#---------------------------------------
set.seed(112233)

library(leaps)
# Load the data or define the cluster1 object

# Split the data into training and test sets
testidC1<- sample(1:nrow(cluster1_filtered), 568)
test_dataC1 <- cluster1_filtered[testidC1, ]
train_dataC1 <- cluster1_filtered[-testidC1, ]

names(train_dataC1)

#################################################################################
#---------------------------------------
# Cluster 1 - LASSO REG
#---------------------------------------
#install.packages("glmnet")
library(glmnet)
# Fit the lasso regression modelin Cluster 1
LASSOfitC1 <- glmnet(model.matrix(Premium ~ . , data = train_dataC1), train_dataC1$Premium, alpha = 1)

# Select the best lambda using cross-validation
cv.outC1 <- cv.glmnet(model.matrix(Premium ~ . , data = train_dataC1), train_dataC1$Premium, alpha = 1)

best_lambda1 <- cv.outC1$lambda.min

# Obtain the coefficients for the selected lambda
coef_lasso <- coef(LASSOfitC1, s = best_lambda1)

# Calculate predictions on the test data set
test_data_mat5 <- model.matrix(Premium ~ ., data = test_dataC1)
yhat_lasso <- predict(LASSOfitC1, newx = test_data_mat5, s = best_lambda1)

# Compute the test RMSE
test_rmse_lasso <- sqrt(mean((test_dataC1$Premium - yhat_lasso)^2))
tes_mse_lasso <- mean((test_dataC1$Premium - yhat_lasso)^2)
tes_mse_lasso  # 0.009676096

# Report the lambda that was selected and the test RMSE
cat("The best lambda is:", best_lambda1, "\n")
cat("The test RMSE is:", test_rmse_lasso, "\n")

#The best lambda is: 0.002281512 
#The test RMSE is: 0.09836715 
####################################################################################
#---------------------------------------
# Cluster 1 - RIDGE REG
#---------------------------------------

# Fit the ridge regression model in Cluster 1
fit <- glmnet(model.matrix(Premium ~ .,  data = train_dataC1), train_dataC1$Premium, alpha = 0)
# Select the best lambda using cross-validation
cv.out <- cv.glmnet(model.matrix(Premium ~ .,data = train_dataC1),train_dataC1$Premium, alpha = 0)
best_lambda <- cv.out$lambda.min

# Calculate predictions on the test data set
test_data_mat4 <- model.matrix(Premium ~ ., data = test_dataC1)
yhat_ridge <- predict(fit, newx = test_data_mat4, s = best_lambda)

# Compute the test RMSE
test_rmse <- sqrt(mean((yhat_ridge - test_dataC1$Premium)^2))

tes_mse <- mean((yhat_ridge - test_dataC1$Premium)^2)
tes_mse  #0.009852175
# Report the lambda that was selected and the test RMSE
cat("The best lambda is:", best_lambda, "\n")
cat("The test RMSE is:", test_rmse, "\n")

#The best lambda is: 0.006497827 
#The test RMSE is: 0.09925812 
#######################################################################################
#---------------------------------------
# Cluster 1 - FWD SEL
#---------------------------------------
library(caret)
linear_combos1 <- findLinearCombos(train_dataC1)
train_dataC1 <- train_dataC1[, -linear_combos1$remove]
test_dataC1 <- test_dataC1[, -linear_combos1$remove]

# Perform forward stepwise regression on the training data set
stepwise_FC1 <- regsubsets(Premium ~ ., data = train_dataC1, nvmax = 20, method = "forward")

# Select the "good" model
summary_outputFC1 <- summary(stepwise_FC1)
best_modelFC1 <- which.max(summary_outputFC1$rsq)

# Obtain the coefficients for the selected model
coef_best_modelFC1 <- coef(stepwise_FC1, best_modelFC1)

# Calculate predictions on the test data set
test_data_matFC1 <- model.matrix(Premium ~ ., data = test_dataC1)
yhatFC1 <- test_data_matFC1[, names(coef_best_modelFC1)] %*% coef_best_modelFC1

# Calculate the test RMSE for the selected model
MSE_fwdFC1 <- mean((test_dataC1$Premium - yhatFC1)^2)
RMSE_fwdFC1 <- sqrt(MSE_fwdFC1)
MSE_fwdFC1 # 0.009550645
RMSE_fwdFC1 # RMSE_fwdFC1 = 0.0977274

##################################################################################
#---------------------------------------
# Cluster 1 - BACK SEL
#---------------------------------------
# Perform backward stepwise regression on the training data set
stepwise_BC1 <- regsubsets(Premium ~ ., data = train_dataC1, nvmax = 20, method = "backward")

# Select the "good" model
summary_outputBC1 <- summary(stepwise_BC1)
best_modelBC1 <- which.max(summary_outputBC1$rsq)

# Obtain the coefficients for the selected model
coef_best_modelBC1 <- coef(stepwise_BC1, best_modelBC1)

# Calculate predictions on the test data set
test_data_matBC1 <- model.matrix(Premium ~ ., data = test_dataC1)
yhatBC1 <- test_data_matBC1[, names(coef_best_modelBC1)] %*% coef_best_modelBC1

# Calculate the test RMSE for the selected model
MSE_bwdBC1 <- mean((test_dataC1$Premium - yhatBC1)^2)
RMSE_bwdBC1 <- sqrt(MSE_bwdBC1)
RMSE_bwdBC1   #0.09795401
MSE_bwdBC1  #0.009594988

#######################################################################################
#---------------------------------------
# Cluster 2
#---------------------------------------

set.seed(112233)

# Load the data or define the cluster2 object

# Split the data into training and test sets
testidC2<- sample(1:nrow(cluster2_filtered), 707)
test_dataC2 <- cluster2_filtered[testidC2, ]
train_dataC2 <- cluster2_filtered[-testidC2, ]

#####################################################################################
#---------------------------------------
# Cluster 2 - LASSO REG
#---------------------------------------
# Fit the lasso regression modelin Cluster 2
LASSOfitC2 <- glmnet(model.matrix(Premium ~ . , data = train_dataC2), train_dataC2$Premium, alpha = 1)

# Select the best lambda using cross-validation
cv.outC2 <- cv.glmnet(model.matrix(Premium ~ . , data = train_dataC2), train_dataC2$Premium, alpha = 1)

best_lambda2 <- cv.outC2$lambda.min

# Obtain the coefficients for the selected lambda
coef_lasso <- coef(LASSOfitC2, s = best_lambda2)

# Calculate predictions on the test data set
test_data_mat5 <- model.matrix(Premium ~ ., data = test_dataC2)
yhat_lasso <- predict(LASSOfitC2, newx = test_data_mat5, s = best_lambda2)

# Compute the test RMSE
test_rmse_lasso <- sqrt(mean((test_dataC2$Premium - yhat_lasso)^2))
tes_mse_lasso <- mean((test_dataC2$Premium - yhat_lasso)^2)
tes_mse_lasso  # 0.06724067

# Report the lambda that was selected and the test RMSE
cat("The best lambda is:", best_lambda2, "\n")
cat("The test RMSE is:", test_rmse_lasso, "\n")

#The best lambda is: 0.01667394 
#The test RMSE is: 0.2593081  
####################################################################################
#---------------------------------------
# Cluster 2 - RIDGE REG
#---------------------------------------
# Fit the ridge regression model in Cluster 2
fit <- glmnet(model.matrix(Premium ~ .,  data = train_dataC2), train_dataC2$Premium, alpha = 0)
# Select the best lambda using cross-validation
cv.out <- cv.glmnet(model.matrix(Premium ~ .,data = train_dataC2),train_dataC2$Premium, alpha = 0)
best_lambda <- cv.out$lambda.min

# Calculate predictions on the test data set
test_data_mat4 <- model.matrix(Premium ~ ., data = test_dataC2)
yhat_ridge <- predict(fit, newx = test_data_mat4, s = best_lambda)

# Compute the test RMSE
test_rmse <- sqrt(mean((yhat_ridge - test_dataC2$Premium)^2))

tes_mse <- mean((yhat_ridge - test_dataC2$Premium)^2)
tes_mse  #0.07615796
# Report the lambda that was selected and the test RMSE
cat("The best lambda is:", best_lambda, "\n")
cat("The test RMSE is:", test_rmse, "\n")

#The best lambda is: 0.006497827 
#The test RMSE is: 0.2759673
#######################################################################################
#---------------------------------------
# Cluster 2 - FWD SEL
#---------------------------------------
library(caret)
linear_combos <- findLinearCombos(train_dataC2)
train_dataC2 <- train_dataC2[, -linear_combos$remove]
test_dataC2 <- test_dataC2[, -linear_combos$remove]

library(leaps)
# Perform forward stepwise regression on the training data set CLUSTER 2
stepwise_FC2 <- regsubsets(Premium ~ ., data = train_dataC2, nvmax = 20, method = "forward")

# Select the "good" model
summary_outputFC2<- summary(stepwise_FC2)
best_modelFC2<- which.max(summary_outputFC2$rsq)

# Obtain the coefficients for the selected model
coef_best_modelFC2 <- coef(stepwise_FC2, best_modelFC2)

# Calculate predictions on the test data set
test_data_matFC2 <- model.matrix(Premium ~ ., data = test_dataC2)
yhatFC2 <- test_data_matFC2[, names(coef_best_modelFC2)] %*% coef_best_modelFC2

# Calculate the test RMSE for the selected model
MSE_fwdFC2 <- mean((test_dataC2$Premium - yhatFC2)^2)
RMSE_fwdFC2 <- sqrt(MSE_fwdFC2)
MSE_fwdFC2 # 0.07627968
RMSE_fwdFC2 # 0.2761878
##################################################################################
#---------------------------------------
# Cluster 2 - BCK SEL
#---------------------------------------
# Perform backward stepwise regression on the training data set
stepwise_BC2 <- regsubsets(Premium ~ ., data = train_dataC2, nvmax = 20, method = "backward")

# Select the "good" model
summary_outputBC2 <- summary(stepwise_BC2)
best_modelBC2 <- which.max(summary_outputBC2$rsq)

# Obtain the coefficients for the selected model
coef_best_modelBC2 <- coef(stepwise_BC2, best_modelBC2)

# Calculate predictions on the test data set
test_data_matBC2 <- model.matrix(Premium ~ ., data = test_dataC2)
yhatBC2 <- test_data_matBC2[, names(coef_best_modelBC2)] %*% coef_best_modelBC2

# Calculate the test RMSE for the selected model
MSE_bwdBC2 <- mean((test_dataC2$Premium - yhatBC2)^2)
RMSE_bwdBC2 <- sqrt(MSE_bwdBC2)
RMSE_bwdBC2  #0.2750532
MSE_bwdBC2  #0.07565425

###########################################################################################
#---------------------------------------
# Cluster 1 - regsubsets
#---------------------------------------
# Perform regsubsets on the training data set
regsubsets_output <- regsubsets(Premium ~ ., data = train_dataC1, nvmax = 20, really.big = T)

# Select the "good" model
summary_output <- summary(regsubsets_output)
best_modelT <- which.max(summary_output$rsq)
plot(summary(regsubsets_output)$rsq)

# Obtain the coefficients for the selected model
coef_best_model <- coef(regsubsets_output, best_modelT)

# Calculate predictions on the test data set
test_data_mat <- model.matrix(Premium ~ ., data = test_dataC1)
yhat <- test_data_mat[, names(coef_best_model)] %*% coef_best_model

# Calculate the test RMSE for the selected model
MSE <- mean((test_dataC1$Premium - yhat)^2)
RMSE <- sqrt(MSE)
MSE  # 0.009761866
RMSE #0.09880216
###########################################################################################
#---------------------------------------
# Cluster 2 - regsubsets
#---------------------------------------
# Perform regsubsets on the training data set
regsubsets_output <- regsubsets(Premium ~ ., data = train_dataC2, nvmax = 8, really.big = T)

# Select the "good" model
summary_output <- summary(regsubsets_output)
best_modelT <- which.max(summary_output$rsq)
plot(summary(regsubsets_output)$rsq)

# Obtain the coefficients for the selected model
coef_best_model <- coef(regsubsets_output, best_modelT)

# Calculate predictions on the test data set
test_data_mat <- model.matrix(Premium ~ ., data = test_dataC2)
yhat <- test_data_mat[, names(coef_best_model)] %*% coef_best_model

# Calculate the test RMSE for the selected model
MSE <- mean((test_dataC2$Premium - yhat)^2)
RMSE <- sqrt(MSE)
MSE  #  0.06962937
RMSE #0.2638738

# Load required libraries
library(caret)

#---------------------------------------
# Cluster 1
#---------------------------------------
# Subset data for PCA
cluster1_pca <- subset(cluster1, select = -Premium)

# Scale data for PCA
cluster1_pca_scaled <- scale(cluster1_pca)

# Perform PCA
cluster1_pca_results <- prcomp(cluster1_pca_scaled)

# Extract principal components with eigenvalues greater than 1
cluster1_pca_loadings <- cluster1_pca_results$rotation[, which(cluster1_pca_results$sdev >= 1)]

# Transform data using principal components
cluster1_pca_transformed <- as.data.frame(cluster1_pca_scaled %*% cluster1_pca_loadings)

# Add back in response variable
cluster1_pca_transformed$Premium <- cluster1$Premium

# Split data into test and train sets
set.seed(123)
trainIndex <- createDataPartition(cluster1_pca_transformed$Premium, p = 0.7, list = FALSE)
train <- cluster1_pca_transformed[trainIndex, ]
test <- cluster1_pca_transformed[-trainIndex, ]

# Perform forward stepwise regression on the training data set
stepwise_BC1 <- regsubsets(Premium ~ ., data = train, nvmax = 20, method = "forward")

# Select the "best" model
summary_outputBC1 <- summary(stepwise_BC1)
best_modelBC1 <- which.max(summary_outputBC1$rsq)

# Obtain the coefficients for the selected model
coef_best_modelBC1 <- coef(stepwise_BC1, best_modelBC1)

# Calculate predictions on the test data set
test_data_matBC1 <- model.matrix(Premium ~ ., data = test)
yhatBC1 <- test_data_matBC1[, names(coef_best_modelBC1)] %*% coef_best_modelBC1

# Calculate the test RMSE for the selected model
MSE_BC1 <- mean((test$Premium - yhatBC1)^2)
RMSE_BC1 <- sqrt(MSE_BC1)
cat("Cluster 1 Test RMSE: ", RMSE_BC1, "\n")    # 0.1440614

##################################################################################
# Load libraries
library(tidyverse)
library(caret)

# Create formula for linear regression
formula <- "premium ~ . - sales_price"

# Feature selection using caret package
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
lmProfile <- train(formula, data = cluster1, method = "lm", trControl = ctrl)
important_features <- varImp(lmProfile)

# Create final formula for linear regression by selecting important features
final_formula <- paste("premium ~", paste(names(important_features$importance)[1:4], collapse = " + "))

# Perform linear regression for cluster 1
lm_cluster1 <- lm(final_formula, data = data_cluster1)

# Cross-validation using caret package
set.seed(123)
lm_cv_cluster1 <- train(final_formula, data = data_cluster1, method = "lm", trControl = ctrl)

# Perform linear regression for cluster 2
lm_cluster2 <- lm( , data = data_cluster2)

# Cross-validation using caret package
set.seed(123)
lm_cv_cluster2 <- train(final_formula, data = data_cluster2, method = "lm", trControl = ctrl)

# Plot the linear regression models for cluster 1
plot(lm_cluster1, col = "red")
plot(lm_cv_cluster1$finalModel, add = TRUE, col = "blue")

# Plot the linear regression models for cluster 2
plot(lm_cluster2, col = "red")
plot(lm_cv_cluster2$finalModel, add = TRUE, col = "blue")


# We try different approach, CEO's domain knowledge model and Lasso regression model

a <- read.csv("real_final2.csv")
names(a)

# Remove all sales related data so we can test on new cars without sales record

a <- a[c(1,3,5:6,8:10,27:87,112:147)]

names(a)

### For the purpose of pre study, we buil models on the entire data set, ignoring clusters, first

### Approach 1, feature selection with CEO's domain knowledge

# Assume that he suggested MMR_price, Age, awarded, odometer, condition, and transmission to consider

# Train, Test data split

set.seed(123)
train.id <- sample(1:nrow(a), nrow(a)*0.7)
train <- a[train.id,]
x.train <- train[-1]
y.train <- train[[1]]
test <- a[-train.id,]
x.test <- test[-1]
y.test <- test[[1]]

# Model building

fit.dn <- lm(Premium ~ MMR_price + Age + awarded + odometer + condition + transmission, train)
summary(fit.dn)

# R2 of 0.2199, RMSE is 0.1764497

summary(fit.dn)
y_hat <- predict(fit.dn, x.test)
mse(y_hat, y.test)^0.5

# Build this model on the entire data set 

fit.ceo <- lm(Premium ~ MMR_price + Age + awarded + odometer + condition + transmission, a)

# Final R2 is 0.2226, RMSE is 0.1748132, we will compare this to the lasso model
summary(fit.ceo)
y_hat <- predict(fit.ceo, a)
mse(y_hat, a[[1]])^0.5


### Approach 2, Lasso regression 

# Find the best lambda first, 0.01

cv.out0 <- cv.glmnet(as.matrix(x.test), y.test, alpha = 1)
plot(cv.out0)
bestlam0 <- cv.out0$lambda.min
bestlam0

# Build the model with this lambda

lasso.c0 <- glmnet(as.matrix(x.train), y.train, alpha = 1, lambda = bestlam0)


#### Test for the graph

grid <- 10^seq(10,-2,length=100)

lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid, thresh = 1e-12)

plot(lasso.mod, xvar="lambda", label = TRUE)

for (j in 1:100) {
  ridge.coeff[,j] <- ridge.mod$beta[,j]
  ridge.pred[,j] <- predict(ridge.mod, s = grid[j], 
                            newx = X.test)
  testerr[j] <- mean((ridge.pred[,j] - y.test)^2)
}


# Make a prediction and calculate the RMSE, 0.1745793

newX <- cbind(1, as.matrix(x.test))
yhat0 <- newX%*%coef(lasso.c0)[,1]
RSS.L.best <- sum((y.test - yhat0)^2)
RSS.L.best
MSE.L.best <- RSS.L.best/(lasso.c0$nobs-lasso.c0$df)
MSE.L.best
RMSE.L.best <- MSE.L.best^0.5
RMSE.L.best
RSQ.L.best <- RSS.L.best/sum((y.test-mean(y.test))^2)
RSQ.L.best

# R-squared is 0.6531, we can see that Lasso model performs better than the CEO's model



### Now we build the models for each cluster

### Cluster 1

# Prepare cluster1 data... after filtering, remove cluster number
a1 <- a[a$ClusterNo==1,]
a1 <- a1[-104]

### Train, Test data split

set.seed(123)
train.id <- sample(1:nrow(a1), nrow(a1)*0.7)
train <- a1[train.id,]
x.train <- train[-1]
y.train <- train[[1]]
test <- a1[-train.id,]
x.test <- test[-1]
y.test <- test[[1]]


### 1. Again, we first try based on CEO's domain knowledge 

fit0 <- lm(Premium ~ MMR_price + Age + awarded + odometer + condition + transmission, train)

# We got R2 of 0.3512
summary(fit0)

# We got RMSE of 0.09738 on the test data... very precise

y_hat <- predict(fit0, x.test)
mse(y_hat, y.test)^0.5

# Now we build the CEO model on the whole a1 dataset

fit.ceo1 <- lm(Premium ~ MMR_price + Age + awarded + odometer + condition + transmission, a1)

# Final R2 is 0.3545, and RMSE is 0.09959
summary(fit.ceo1)
y_hat_c1 <- predict(fit.ceo1, a1)
mse(y_hat_c1, a1[[1]])^0.5


### 2. Lasso regression modeling

library(glmnet)

cv.out1 <- cv.glmnet(as.matrix(train[-1]), train[[1]], alpha = 1)
plot(cv.out1)
bestlam1 <- cv.out1$lambda.min
bestlam1

# best lamda is 0.00242... almost zero

# Build the model with this lambda

lasso.c1 <- glmnet(x.train, y.train, alpha = 1, lambda = bestlam1)

# Make a prediction and calculate the RMSE, 0.1745793

newX <- cbind(1, as.matrix(x.test))
yhat1 <- newX%*%coef(lasso.c1)[,1]
RSS.L.best <- sum((y.test - yhat1)^2)
RSS.L.best
MSE.L.best <- RSS.L.best/(lasso.c1$nobs-lasso.c1$df)
MSE.L.best
RMSE.L.best <- MSE.L.best^0.5
RMSE.L.best
RSQ.L.best <- RSS.L.best/sum((y.test-mean(y.test))^2)
RSQ.L.best

# RMSE is 0.06388, and R-squared is 0.6246

# Now build the lasso model on the entire cluster 1 data

lasso.c1 <- glmnet(a1[-1], a1[[1]], alpha = 1, lambda = bestlam1)
coef(lasso.c1)

newX <- cbind(1, as.matrix(a1[-1]))
yhat1 <- newX%*%coef(lasso.c1)[,1]
RSS.L.best <- sum((a1[[1]] - yhat1)^2)
RSS.L.best
MSE.L.best <- RSS.L.best/(lasso.c1$nobs-lasso.c1$df)
MSE.L.best
RMSE.L.best <- MSE.L.best^0.5
RMSE.L.best
RSQ.L.best <- RSS.L.best/sum((a1[[1]]-mean(a1[[1]]))^2)
RSQ.L.best
# RMSE is 0.098, # R-squared is 0.615385



### Modeling for cluster2

# Prepare cluster2 data... after filtering, remove cluster number

a2 <- a[a$ClusterNo==2,]
a2 <- a2[-104]

### Train, Test data split

set.seed(123)
train.id <- sample(1:nrow(a2), nrow(a2)*0.7)
train <- a2[train.id,]
x.train <- train[-1]
y.train <- train[[1]]
test <- a2[-train.id,]
x.test <- test[-1]
y.test <- test[[1]]

### 1. CEO's model

fit0 <- lm(Premium ~ MMR_price + Age + awarded + odometer + condition + transmission, train)

# We got R2 of 0.2212
summary(fit0)

# We got RMSE of 0.2651377 on the test data... poorer than c1, they are heterogeneous

y_hat <- predict(fit0, x.test)
mse(y_hat, y.test)^0.5

# Build the CEO model on the whole a2 dataset

fit.ceo2 <- lm(Premium ~ MMR_price + Age + awarded + odometer + condition + transmission, a2)

# R2 is 0.2079, 
summary(fit.ceo2)
y_hat_c2 <- predict(fit.ceo2, a2)
mse(y_hat_c2, a2[[1]])^0.5



### 2. Lasso regression

cv.out2 <- cv.glmnet(as.matrix(train[-1]), train[[1]], alpha = 1)
plot(cv.out2)
bestlam2 <- cv.out2$lambda.min
bestlam2

# best lamda is 0.0134777... almost zero

# Build the model with this lambda

lasso.c2 <- glmnet(x.train, y.train, alpha = 1, lambda = bestlam2)

# Make a prediction and calculate the RMSE, 0.1745793

newX <- cbind(1, as.matrix(x.test))
yhat2 <- newX%*%coef(lasso.c2)[,1]
RSS.L.best <- sum((y.test - yhat2)^2)
RSS.L.best
MSE.L.best <- RSS.L.best/(lasso.c2$nobs-lasso.c2$df)
MSE.L.best
RMSE.L.best <- MSE.L.best^0.5
RMSE.L.best
RSQ.L.best <- RSS.L.best/sum((y.test-mean(y.test))^2)
RSQ.L.best

# RMSE is 0.1727, R Squared is 0.80

# Now, we build the lasso model on the entire cluster 2 data

lasso.c2 <- glmnet(a2[-1], a2[[1]], alpha = 1, lambda = bestlam2)
coef(lasso.c2)

newX <- cbind(1, as.matrix(a2[-1]))
yhat2 <- newX%*%coef(lasso.c2)[,1]
RSS.L.best <- sum((a2[[1]] - yhat2)^2)
RSS.L.best
MSE.L.best <- RSS.L.best/(lasso.c2$nobs-lasso.c2$df)
MSE.L.best
RMSE.L.best <- MSE.L.best^0.5
RMSE.L.best
# RMSE of lasso is 0.2524678
RSQ.L.best <- RSS.L.best/sum((a2[[1]]-mean(a2[[1]]))^2)
RSQ.L.best
# R-squared is 0.7659665, much higher, so we chose this one


### Model Building(2) - Classification

c <- a

c$ClusterNo <- c$ClusterNo-1  # Make ClusterNo binary (cluster1 : fail 0, cluster2 : success 1)
c$Premium <- NULL  # delete Premium

c <- c[c(103, 1:102)]   # Change the position of the dependent variable
#c$ClusterNo <-  factor(c$ClusterNo)

names(c)

### Train / Test data split

# Proportion among the clusters
# cluster1(fail) : 65%, cluster2(success): 35%

mean(c$ClusterNo==1)

set.seed(100)

fail <- c[c$ClusterNo==0,]
succ <- c[c$ClusterNo==1,]

nrow(succ)  # 1010, so the half is 505


train.succ <- sample(1:nrow(succ),505)
train.fail <- sample(1:nrow(fail),505)

# This is the train data set
dat.train <- rbind(succ[train.succ,],fail[train.fail,])

(505/0.35) - 505

newfail <- fail[-train.fail,]
test.fail <- newfail[sample(1:nrow(newfail),938),]

# This is the test data set
dat.test <- rbind(succ[-train.succ,],test.fail)

mean(dat.test$ClusterNo)  # The same proportion of the original data set


# We select the top 10 highest correlated variables, which are:
# Age, odometer, MMR_price, condition, int_colblack, int_colgray, nationalKOR, bodySedan, makeInfiniti, bodyCab

#### SVM

# Create data set for SVM building

library(e1071)

set.seed(100)

dat.train$ClusterNo <- factor(dat.train$ClusterNo)

# Find the best svm model

tune.out <- tune(svm, ClusterNo ~ Age + odometer + MMR_price + condition + int_colblack + int_colgray + nationalKOR + bodySedan + makeInfiniti + bodyCab, data = dat.train, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))

# This is the best SVM model

svm.best <- tune.out$best.model

svm.pred <- predict(svm.best, dat.test, type="class")

table(actual=dat.test[[1]], predict=svm.pred)

library(caret)

confusionMatrix(factor(dat.test[[1]]), svm.pred)


### Classify each dealer's cars

# Prepare the data sets of each dealer

# Dealer 1 data set

d1 <- read.csv("1. nv100.csv")
d1 <- d1[c(5,7:8,10:12,27:78,94:119)]
colnames(d1)[1] <- "MMR_price"
d1.clust <- predict(svm.best, d1, type="class")
d1 <- cbind(d1, d1.clust)
colnames(d1)[85] <- "ClusterNo"

# Dealer 1 Pie Chart

d1$ClusterNo <- factor(d1$ClusterNo)
d1.pie <- table(d1$ClusterNo)
pct1 <- round(100 * d1.pie / sum(d1.pie))
labels1 <- sprintf("%s\n%d%%", c("Cluster1", "Cluster2"), pct1)
pie(d1.pie, col=c("lightgray", "black"), init.angle=90, labels=labels1)


# Dealer 2 Data set

d2 <- read.csv("2. tx100.csv")
names(d2)
d2 <- d2[c(4,6,7,9:11, 27:84, 106:135)]
colnames(d2)[1] <- "MMR_price"
d2.clust <- predict(svm.best, d2, type="class")
d2 <- cbind(d2, d2.clust)
colnames(d2)[95] <- "ClusterNo"

# Dealer 2 Pie Chart

d2$ClusterNo <- factor(d2$ClusterNo)
d2.pie <- table(d2$ClusterNo)
pct2 <- round(100 * d2.pie / sum(d2.pie))
labels2 <- sprintf("%s\n%d%%", c("Cluster1", "Cluster2"), pct2)
pie(d2.pie, col=c("lightgray", "black"), init.angle=90, labels=labels2)

# Dealer 3 Data set

d3 <- read.csv("3. ny100.csv")
names(d3)
d3 <- d3[c(4,6,7,9:11, 25:68, 86:108)]
colnames(d3)[1] <- "MMR_price"
d3$makeInfiniti <-  d2$makeInfiniti
d3.clust <- predict(svm.best, d3, type="class")
d3 <- cbind(d3, d3.clust)
colnames(d3)[75] <- "ClusterNo"

# Dealer 3 Pie Chart

d3$ClusterNo <- factor(d3$ClusterNo)
d3.pie <- table(d3$ClusterNo)
pct3 <- round(100 * d3.pie / sum(d3.pie))
labels3 <- sprintf("%s\n%d%%", c("Cluster1", "Cluster2"), pct3)
pie(d3.pie, col=c("lightgray", "black"), init.angle=90, labels=labels3)

# Dealer 4 Data

d4 <- read.csv("4. az100.csv")
names(d4)
d4 <- d4[c(4,6,7,9:11, 25:73, 94:115)]
colnames(d4)[1] <- "MMR_price"
d4.clust <- predict(svm.best, d4, type="class")
d4 <- cbind(d4, d4.clust)
colnames(d4)[78] <- "ClusterNo"

# Dealer 4 Pie Chart

d4$ClusterNo <- factor(d4$ClusterNo)
d4.pie <- table(d4$ClusterNo)
pct4 <- round(100 * d4.pie / sum(d4.pie))
labels4 <- sprintf("%s\n%d%%", c("Cluster1", "Cluster2"), pct4)
pie(d4.pie, col=c("lightgray", "black"), init.angle=90, labels=labels4)


### Predict each supplier's cars' price premium

fit.c1 <- lm(Premium ~ MMR_price + Age + awarded + odometer + condition + transmission, data=a1)

fit.c2 <- lm(Premium ~ MMR_price + Age + awarded + odometer + condition + transmission, data=a2)

#Dealer1

#Cluter1

d1.c1 <- d1[d1$ClusterNo==0,]
pred.d1.c1 <- predict(fit.c1, d1.c1)

#Cluster2

d1.c2 <- d1[d1$ClusterNo==1,]
pred.d1.c2 <- predict(fit.c2, d1.c2)

d1.pred <- c(pred.d1.c1, pred.d1.c2)
mean(d1.pred)  # 1.034343

#Dealer2

#Cluter1

d2.c1 <- d2[d2$ClusterNo==0,]
pred.d2.c1 <- predict(fit.c1, d2.c1)

#Cluster2

d2.c2 <- d2[d2$ClusterNo==1,]
pred.d2.c2 <- predict(fit.c2, d2.c2)

d2.pred <- c(pred.d2.c1, pred.d2.c2)
mean(d2.pred)  # 1.022906

#Dealer3

#Cluter1

d3.c1 <- d3[d3$ClusterNo==0,]
pred.d3.c1 <- predict(fit.c1, d3.c1)

#Cluster2

d3.c2 <- d3[d3$ClusterNo==1,]
pred.d3.c2 <- predict(fit.c2, d3.c2)

d3.pred <- c(pred.d3.c1, pred.d3.c2)
mean(d3.pred)  # 1.024032  Similar to Dealer 2

#Dealer4

###Cluter1

d4.c1 <- d4[d4$ClusterNo==0,]
pred.d4.c1 <- predict(fit.c1, d4.c1)

###Cluster2

d4.c2 <- d4[d4$ClusterNo==1,]
pred.d4.c2 <- predict(fit.c2, d4.c2)

d4.pred <- c(pred.d4.c1, pred.d4.c2)
mean(d4.pred)  # 1.014443
sd(d4.pred) # 0.079


mean(d1.pred)  #1.034
sd(d1.pred)    #0.069

mean(d2.pred)  #1.023
sd(d2.pred)    #0.095

mean(d3.pred)  #1.024
sd(d3.pred)    #0.101

mean(d4.pred)  #1.014
sd(d4.pred)    #0.079

summary(d2.pred)
summary(d4.pred)

mean(d2.pred)

x <- data.frame(Dealer1=d1.pred, Dealer2=d2.pred, Dealer3=d4.pred)

x[x["Dealer2"]==min(x["Dealer2"]),][[2]] <- mean(x[["Dealer2"]])
x[x["Dealer2"]==max(x["Dealer2"]),][[2]] <- mean(x[["Dealer2"]])

boxplot(x, col=c("gray", "white", "white"))