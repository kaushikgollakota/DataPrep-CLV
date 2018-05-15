
#variables to be included in the final model
num_cols_hpmnk <- c("tot_game_play","num_female_child","num_male_child","num_game_bght","num_game_plyd","FrequencyLF","FrequencyApp","UNITS","TOT_REV_GEN")

factor_colnames<-c("CONTACT_WID","NOMIN_DT","OVR_LST_TRANS","X_GAME_NM","fav_source","CHANNEL_DESCRIPTION")



tot_cols_hpmnk <- c("CONTACT_WID","X_GAME_NM","CHANNEL_DESCRIPTION","fav_source","tot_game_play","num_female_child","num_male_child","num_game_bght","num_game_plyd","FrequencyLF","FrequencyApp","UNITS")

factor_cols_hpmnk <- setdiff(intersect(tot_cols_hpmnk,factor_colnames),"CONTACT_WID")

factor_cols_hpmnk_2 <- c("fav_source90","fav_source180","fav_source360","FavChannel90","FavChannel180","FavChannel360",
"FavGameBin90","FavGameBin180","FavGameBin360")

num_cols_hpmnk_2 <-c("Units90","Units180","Units360","FrequencyLF90","FrequencyLF180","FrequencyLF180",
"FrequencyApp90","FrequencyApp180","FrequencyApp360","NumGames90","NumGames180","NumGames360")

tot_cols_hpmnk_2 <- union(num_cols_hpmnk_2,factor_cols_hpmnk_2)
tot_cols_hpmnk_2 <- union(tot_cols_hpmnk_2,c("CONTACT_WID"))
final_data <- merge(hopmonk_data[tot_cols_hpmnk],hopmonk_data_2[tot_cols_hpmnk_2],by="CONTACT_WID")
final_data <- merge(final_data,hopmonk_data[c("CONTACT_WID","TOT_REV_GEN")],by="CONTACT_WID")

#-----------------------
all_factors <- union(factor_cols_hpmnk,factor_cols_hpmnk_2)
all_num <- union(num_cols_hpmnk,num_cols_hpmnk_2)

final_data <- addNA(final_data)
levels(final_data) <- c(levels(final_data), NA)
sum(is.na(final_data))

final_data <- final_data[complete.cases(final_data),]
final_data[is.na(final_data)]<-0
sum(is.na(final_data))
dim(final_data)
final_data$fav_source90 <- as.factor(final_data$fav_source90)
final_data$fav_source180 <- as.factor(final_data$fav_source180)
final_data$fav_source360 <- as.factor(final_data$fav_source360)
str(final_data)
final_data <- subset(final_data,select=-c(CONTACT_WID))
dim(final_data)
#dummifying factors
#paste("~",paste(all_factors,collapse = "+")
dummy_facs <- model.matrix(~+final_data$X_GAME_NM+final_data$CHANNEL_DESCRIPTION+final_data$fav_source+final_data$fav_source90+final_data$fav_source180+final_data$fav_source360+final_data$FavChannel90+final_data$FavChannel180+final_data$FavChannel360+final_data$FavGameBin90+final_data$FavGameBin180+final_data$FavGameBin360)[,-1]

num_vars <- final_data[,all_num]

rows=seq(1,nrow(final_data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(final_data))/100)
train = data.frame(num_vars, dummy_facs,TotalRevenueGenerated=final_data$TOT_REV_GEN)[trainRows,]
test = data.frame(num_vars, dummy_facs,TotalRevenueGenerated=final_data$TOT_REV_GEN)[-trainRows,]
"
#----only num
num_vars <- final_data[,all_num]
final_data <- num_vars
final_data <- final_data[complete.cases(final_data),]

rows=seq(1,nrow(final_data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(final_data))/100)
train = data.frame(final_data[all_num],TotalRevenueGenerated=final_data$TOT_REV_GEN)[trainRows,]
test = data.frame(final_data[all_num], TotalRevenueGenerated=final_data$TOT_REV_GEN)[-trainRows,]
"
#onlynum

library(glmnet)

system.time(LinReg<- lm(TotalRevenueGenerated ~ ., data=train) ) 
summary(LinReg)
plot(LinReg)
#Evaluation of the model: Error metrics
library(DMwR)
regr.eval(train$TotalRevenueGenerated, predict(LinReg,train))
regr.eval(test$TotalRevenueGenerated, predict(LinReg,test))

#glm

data_mat <- as.matrix(data.frame(num_vars, dummy_facs))
train = data_mat[trainRows,] 
test = data_mat[-trainRows,]

#Target Varaible
ytrain=final_data$TotalRevenueGenerated[trainRows]
ytest = final_data$TotalRevenueGenerated[-trainRows]
#############################################################################
# Ridge Regression  using glmnet  - L2 norm
library(glmnet)
# fit model
fit_ridge <- glmnet(train,ytrain,alpha=0)
plot(fit_ridge,xvar="lambda",label=TRUE)

#Model Selection
coef(fit_ridge) 
cv.ridge=cv.glmnet(train,y,alpha=0)
plot(cv.ridge)
coef(cv.ridge)

regr.eval(y, predict(fit2,train))
regr.eval(ytest, predict(fit2,test))

regr.eval(y, predict(fit1,train))
regr.eval(ytest, predict(fit1,test))


# Lasso Regression  using glmnet - L1 norm
# fit model
fit_lasso <- glmnet(train,y, alpha=1)

plot(fit_lasso,xvar="lambda",label=TRUE)
plot(fit_lasso,xvar="dev",label=TRUE)

#Model Selection
coef(fit_lasso)
cv.lasso=cv.glmnet(train,y)
plot(cv.lasso)
coef(cv.lasso)




