glm1 <- function (formula,data=list(),...) {
    UseMethod("glm1", data)
}

#' @export
glm1.default <- function (formula,data=list(),...) {
    return(stats::glm(formula,data=data,...))
}

#'fltbl <- FLTable("tblDrugWide", "OBSID")
#' flmod <- glm1(Dep = "Effect", Categor = c("DRUG", "DISEASE"),Interaction = list(c("DRUG","DISEASE"), c("DISEASE","TRIAL")),data = fltbl )
glm1.FLTable <- function(Dep, Categor, Interaction= list(),data=list(),...){
    browser()
    if(isDeep(data)){
        stop("only works for wide-table as of now")
    }
    vformula <- formula
    vcall <- match.call()
    vstr <- paste0("DELETE FROM fzzlGLMColumns WHERE SPECID IN (",
                   paste0("'GLM",1:length(categ),"'",collapse ="," ),");")
    vcheck <- sqlSendUpdate(connection, vstr)
    
    vstr <- paste0("DELETE FROM fzzlGLMIntColumns WHERE SPECID IN (",
                   paste0("'INT",1:length(Interaction),"'",collapse ="," ),");")
    vcheck <- sqlSendUpdate(connection, vstr)
    var <- lapply(1:length(Interaction), function(i){
        paste0("INSERT INTO fzzlGLMIntColumns VALUES ('INT",i,"',",i,",",
               paste0(fquote(Interaction[[i]]), collapse = ","),")") })
    vcheck <- sqlSendUpdate(connection , var)
    var <- lapply(1:length(Categor), function(i){
        paste0("INSERT INTO fzzlGLMColumns VALUES ('GLM",i,"',",i,",",fquote(Categor[i]),")") })
    vcheck <- sqlSendUpdate(connection , var)
    functionName <- "FLGLM"
    vinputcols <- c(TableName = data@select@table_name,
                    RecIDCOl = 'ObsID',
                    DepVarCol = Dep,
                    COLSPECID = 'GLM2',
                    IntSPECID = 'Int1',
                    LSMeansCol ='Drug' ,
                    Note = "glm for adapteR")
    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams = vinputcols,
                         outputParameter = c(OutTable = 'a')
                         )
    vAnalysisID <- ret[[1]]
    vdf <- sqlQuery(connection, paste0("SELECT 
FROM fzzlGLMRegrCoeffs a
WHERE a.AnalysisID = 
ORDER BY 2, 3;"))
}
