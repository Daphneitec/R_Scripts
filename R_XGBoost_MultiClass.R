#Define the connection string to connect to the TutorialDB database
connStr <- "Driver=SQL Server;Server=MatrixSQLBI;Database=SCI_ODS_PROD;uid=MatrixTableau;pwd=M@tr1xSCI"


#Get the data from the table
library(RODBC)
library(xgboost)
library(data.table)

ch <- odbcDriverConnect(connStr)

#Import the data from the table
#select LANE_NAME,MODE_SERVICE_LEVEL_TYPE,O_ZIP, D_ZIP, Transit_Days, datasplit from dbo.v_GTC_Transitdays_Training
mydata <- sqlQuery(ch, "select LANE_NAME,MODE_SERVICE_LEVEL_TYPE,O_ZIP, D_ZIP, Transit_Days, datasplit from dbo.v_GTC_Transitdays_Training where Transit_Days < 14")

#Take a look at the structure of the data and the top rows
head(mydata)

str(mydata)

train = mydata[mydata$datasplit == "Train",]
test  = mydata[mydata$datasplit == "Test",]
train <- data.table(train)
test <- data.table(test)
#convert data frame to data table
train[is.na(train)] <- "Missing" 
test[is.na(test)] <- "Missing"


#using one hot encoding for dimensions 
labels <- train$Transit_Days
ts_label <- test$Transit_Days
new_tr <- model.matrix(~.+0,data = train[,-c("Transit_Days","datasplit")]) 
new_ts <- model.matrix(~.+0,data = test[,-c("Transit_Days","datasplit")])

#head(new_tr)
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

params <- list(booster = "gbtree", objective = "multi:softmax",num_class=14, eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)


xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every_n = 1, early_stopping_rounds = 20, maximize = F)


