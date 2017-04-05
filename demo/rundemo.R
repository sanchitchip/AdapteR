library(pROC)
library(randomForest)
if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}

## Standard Demo for Creditcard type USe-Case
## just insert the table-name which can be Deep or Wide.

rundemo <- function(formula = Classvar ~ ., tablename = "ARcreditcard" ...){
#############################################################
    vtraintbl <-paste(tablename,"train",sep = "" )
    vtesttbl <- paste(tablename,"test",sep = "" )
    vSampleDataTables <- suppressWarnings(SampleData(pTableName=tablename,
                                                     pObsIDColumn="ObsID",
                                                     pTrainTableName=vtraintbl,
                                                     pTestTableName=vtesttbl,
                                                     pTrainDataRatio=.60,
                                                     pTemporary=FALSE,
                                                     pDrop=TRUE))
    vTrainTableName <- vSampleDataTables["TrainTableName"]
    vTestTableName <- vSampleDataTables["TestTableName"]


    vtemp <- readline("Above: Using SampleData to create Train & Test Data\n ")


    ## Create a FLTable object for Training table
    FLtbl <- FLTable(vTrainTableName,"ObsID",fetchIDs=FALSE)
    FLTestTbl <- FLTable(vTestTableName,"ObsID",fetchIDs=FALSE)

    myformula <- formula
    vdependentColumn <- as.character(vformula)[[2]]


    deepTableName<- paste0(vTrainTableName, "deeptbl", sep = "")
    dropTable(deepTableName)
    if(!existsRemoteTable(tableName=deepTableName)){
        FLtrainDeep <- prepareData(formula         = myformula ,
                                   data            = FLtbl,
                                   outDeepTable    = deepTableName,
                                   makeDataSparse  = 1,
                                   performVarReduc = 0,
                                   minStdDev       = .01,
                                   maxCorrel       = .8,
                                   fetchIDs        = FALSE)
    } else {
        ## or you can use an already created deep table again:
        FLtrainDeep <- FLTable(deepTableName,
                               obs_id_colname   = 'obsid',    
                               var_id_colnames  = 'varid', 
                               cell_val_colname = 'numval',
                               fetchIDs = FALSE)
    }

    FLtestDeep <- prepareData(FLtrainDeep,data=FLTestTbl)

    ## glm model , plot with auc.
    glm.model <- glm(myformula, data = FLtrainDeep, family = "binomial")

    glm.predict <- predict(glm.model,FLtestDeep)

    head(glm.predict, display = TRUE, n = 5)
    glm.roc <- roc(FLtbl$Classvar, glm.predict)
    plot(glm.roc, limit = 1000, main = "glm-roc")

    ## Decision Tree.
    ## change purity level  -> .999
    dt.model <- rpart(myformula,data = FLtrainDeep, control = c(minsplit = 15, cp = .9999, maxdepth = 10))
    dt.predict <- predict(dt.model,type = "prob")
    dt.roc <- roc.FLVector(FLtbl$Classvar, dt.predict)
    plot(dt.roc, limit = 1000, main = "dt-roc", method = 0)


    ## RF:
    rf.model <- randomForest(myformula,data = FLdeepTable, control = c(minsplit = 15, cp = .9999, maxdepth = 10))
    rf.predict <- predict(rf.model,type = "prob")
    rf.roc <- roc(FLtbl$Classvar, rf.predict$prob)
    rf.plot <- plot(rf.roc, limit = 1000, main = "rf-roc")

    ## Bagging:
    bag.model <- bagging(myformula,data = FLdeepTable, control = c(minsplit = 15, cp = .9999, maxdepth = 10))
    bag.predict <- predict(bag.model,type = "prob")
    bag.roc <- roc(FLtbl$Classvar, bag.predict$prob)
    plot(bag.roc, limit = 1000, main = "bag-roc", method = 0)

    ##No probablities in Boosting 
    ## Boosting
    boost.model <- boosting.FLpreparedData(myformula,data = FLtrainDeep, control = c(minsplit = 15, cp = .9999, maxdepth = 10))
    boost.predict <- predict(boost.model)

    ##
#### Random Forest:
    rf.model <- randomForest(myformula,  data = FLtrainDeep, minsplit = 15, cp = .9999, maxdepth = 7)
    rf.predict <- predict(rf.model,type = "prob")
    length(rf.predict)
    rf.roc <- roc.FLVector(FLtbl$Classvar, rf.predict)
    plot(rf.roc, limit = 1000, main = "rf-roc", method = 0)
}
