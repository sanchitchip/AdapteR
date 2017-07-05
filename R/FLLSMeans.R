##setClass("FLLSMeans",
##         slots = list(results = "list",
##                      ))
##


#' @export
lsmeans <- function (formula,data = list(), specs,...) {
    UseMethod("lsmeans", data)
}
#'fltbl <- FLTable("tblLSMeansDeep", "ObsIDCol","VarIDCol","ValueCol")
#' lmfit <- lm(formula= a~., data = fltbl)
#' flmod <- lsmeans(object = lmfit, data = fltbl, specs = 1)
#' rtbl<-fiber
#' fltbl<-as.FLTable(rtbl)
#' lmfit <- lm(formula= strength~machine+diameter, data = fltbl)
#' flmod <- lsmeans(object = lmfit, data = fltbl, specs = "diameter")
#' 
#' @export
lsmeans.FLTable <- function(object, data,specs, ...){
    browser()
    if(is.null(object@AnalysisID) && is.null(object@AnalysisID))
        stop("the model that you mentioned isnt in the database")
    deeptblname <- gen_unique_table_name("lsmeans")
    vout <- gen_unique_table_name("outputlsmeans")
    vAnalysisID <- object@AnalysisID
    if(!isDeep(data))
    {
        FLdeep <- prepareData(formula         = object@formula ,
                              data            = data,
                              outDeepTable    = deeptblname,
                              makeDataSparse  = 1,
                              performVarReduc = 0,
                              minStdDev       = .01,
                              perfromNorm = 1,     
                              maxCorrel       = .8,
                              fetchIDs        = FALSE
                              )
        vdeeptbl <- FLdeep$deepx
        cnames <- c(InTableName =  FLdeep$deepx@select@table_name[[1]],
                    ObsIDCol = FLdeep$deepx@select@variables$obs_id_colname,
                    VarIDCol = FLdeep$deepx@select@variables$var_id_colname,
                    ValueCol= FLdeep$deepx@select@variables$cell_val_colname) }
    else
    { cnames <- c(TableName =  data@select@table_name[[1]],
                  ObsIDCol = gsub("flt.", "",data@select@variables$obs_id_colname),
                  VarIDCol = gsub("flt.", "",data@select@variables$var_id_colname),
                  ValueCol= gsub("flt.", "",data@select@variables$cell_val_colname)) }
    if(is.numeric(specs))
    {
        RefVar1 <- as.numeric(specs)
        RefVar2 <- list(NULL)
    }
    else{
        if(is.character(typeof(specs))) {
            RefVar1 <- as.numeric(FLdeep$vmapping[[specs]])
            RefVar2 <- as.numeric(FLdeep$vmapping[[specs]]) + 1}
        else {
            RefVar1 <- 1
            RefVar2 <- list(NULL)
       } 
    }
    cnames <- c(cnames,
                RegrAnalysisID = vAnalysisID,
                RefVar1 = RefVar1,
                RefVar2 = RefVar2,
                OutTableName = vout)
    functionName <- "FLLSMeans"
    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams = cnames,
                         outputParameter = c(OutTable = 'a')
                         )
    print(ret)

    
}
