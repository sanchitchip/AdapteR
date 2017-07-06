setClass("FLLSMeans",
         slots = list(call = "call",
                      table = "FLTable",
                      deeptable = "FLTable",
                      results = "list" ))

#' @export
lsmeans <- function (object,data = list(), specs,...) {
    UseMethod("lsmeans", data)
}

#' @export
lsmeans.default <- function(object, specs, ...){
    if (!requireNamespace("lsmeans", quietly = TRUE)){
        stop("lsmeans package needed for lsmeans. Please install it.",
             call. = FALSE)
    }
    else return(lsmeans::lsmeans(object, specs, ...)) }

#'fltbl <- FLTable("tblLSMeansDeep", "ObsIDCol","VarIDCol","ValueCol")
#' lmfit <- lm(formula= a~., data = fltbl)
#' flmod <- lsmeans(object = lmfit, data = fltbl, specs = 1)
#'rtbl <- as.R(fltbl);rtbl<-rtbl[,-2]
#' colnames(rtbl) <- letters[1:4]
#' rfit <- lm(formula= a~., data = rtbl)
#' rmod <- lsmeans(rfit,specs = "b")
#' 
#' rtbl<-fiber
#' fltbl<-as.FLTable(rtbl)
#' lmfit <- lm(formula= strength~machine+diameter, data = fltbl)
#' rfit <- lm(formula= strength~machine+diameter, data = rtbl)
#' flmod <- lsmeans(object = lmfit, data = fltbl, specs = "machine")
#' rmod <- lsmeans(object = rfit, data = rtbl, specs = "machine")
#'
#' @export
lsmeans.FLTable <- function(object, data,specs, ...){
    vcallObject <- match.call()  
    if(is.null(object@AnalysisID) && is.null(object@AnalysisID))
        stop("the model that you mentioned isnt in the database")
    if(!any(colnames(data) == specs))
        stop("column name mentioned in specs doesn't exist")
    deeptblname <- gen_unique_table_name("lsmeans")
    vout <- gen_unique_table_name("outputlsmeans")
    vAnalysisID <- object@AnalysisID
    vtbl <- data
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
        vtbl <- FLdeep$deepx
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
        if(typeof((data[[specs]])) != "character"){
            RefVar1 <- as.numeric(specs)
            RefVar2 <- list(NULL)
            vind <- which(colnames(data)==specs)
            vmean <- mean(data[[vind]])
        }
        
        else
        {
            stop("dont compute for categorical variable with numerical names")
        }
    }
    else{
        if(typeof(data[[specs]]) != "character") {
            RefVar1 <- 1
            RefVar2 <- list(NULL)
            vmean <- mean(data[[specs]])
            
        }
        else {
            vindex <- which(names(FLdeep$vmapping) == specs)          
            RefVar1 <- as.numeric(FLdeep$vmapping[[specs]])
            RefVar2 <-max(FLdeep$vmapping[vindex])
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
    rdf <- sqlQuery(connection, paste0("select RefVarID as Var,LSmean as lsmean, StdErr as SE from ",vout," order by 1 ASC"))
    if(exists("vmean")){
        rdf <- data.frame(mean = vmean,rdf)
        rdf <- subset(rdf, select = -Var)
        colnames(rdf)[1] <- specs }    
    
    return(new("FLLSMeans",
        call = vcallObject,
        table = data,
        deeptable = vtbl,
        results = list(rdf = rdf )))
    }




print.FLLSMeans <- function(object,...){
    print(object@results$rdf)
}



#' @export
setMethod("show", signature("FLLSMeans"), function(object){
    print.FLLSMeans(object)})





#' @export
`$.FLLSMeans`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "val")
        return(object@results$rdf)
}
