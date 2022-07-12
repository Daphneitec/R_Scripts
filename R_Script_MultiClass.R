#Define the connection string to connect to the TutorialDB database
connStr <- "Driver=SQL Server;Server=MatrixSQLBI;Database=SCI_ODS_PROD;uid=MatrixTableau;pwd=M@tr1xSCI"


#Get the data from the table
library(RODBC)

ch <- odbcDriverConnect(connStr)

#Import the data from the table
mydata <- sqlFetch(ch, "dbo.v_GTC_Transitdays_Training")

#Take a look at the structure of the data and the top rows
head(mydata)

str(mydata)

train_data = mydata[mydata$datasplit == "Train",]
test_data  = mydata[mydata$datasplit == "Test",]
#head(train_data)
#head(test_data)
#Model 1: Use lm to create a linear regression model, trained with the training data set
model_lm <- lm(Transit_Days ~  O_STATE + D_STATE + LANE_NAME + MODE_SERVICE_LEVEL_TYPE, data = train_data)
predict_lm <- predict(model_lm, test_data)
predict_lm <- data.frame(Transit_Days_Pred = predict_lm, Transit_Days = test_data$Transit_Days, 
                         O_State = test_data$O_STATE, D_STATE = test_data$D_STATE,
                         LANE_NAME = test_data$LANE_NAME, MODE_SERVICE_LEVEL_TYPE = test_data$MODE_SERVICE_LEVEL_TYPE)
print(predict_lm)

library(rpart)
model_rpart  <- rpart(Transit_Days ~  O_STATE + D_STATE + LANE_NAME + MODE_SERVICE_LEVEL_TYPE, data = train_data)
predict_rpart  <- predict(model_rpart,  test_data)
predict_rpart <- data.frame(Transit_Days_Pred = predict_rpart,Transit_Days = test_data$Transit_Days, 
                            O_STATE = test_data$O_STATE, D_STATE = test_data$D_STATE,
                            LANE_NAME = test_data$LANE_NAME, MODE_SERVICE_LEVEL_TYPE = test_data$MODE_SERVICE_LEVEL_TYPE)
print(predict_rpart)
