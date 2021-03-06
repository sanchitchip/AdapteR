# Functions to check the type of the object


#' Check if the object is an FLMatrix object
#' @export
is.FLMatrix <- function(object)
{
    if (class(object) == "FLMatrix" |
        class(object) == "FLMatrixBind")
        return (TRUE)
	else return (FALSE)
}

#' Check if the object is an FLVector object
#' @export
is.FLVector <- function(object)
{
	ifelse(class(object)=="FLVector",TRUE,FALSE)
	
}

#' Check if the object is an FLTable object
#' @export
is.FLTable <- function(object)
{
	ifelse(class(object)=="FLTable",TRUE,FALSE)
}

#' @export
is.FLTableMD <- function(object)
{
    ifelse(class(object)=="FLTableMD",TRUE,FALSE)
}

is.FLAbstractColumn <- function(object){
    if(class(object)=="FLAbstractColumn")
    return(TRUE)
    else return(FALSE)
}

is.RowFLVector <- function(pObject){
    if(is.FLVector(pObject) && 
        !pObject@isDeep && 
            ncol(pObject)>1)
    return(TRUE)
    else return(FALSE)
}

is.wideFLTable <- function(pObject){
    if(!is.FLTable(pObject))
    return(FALSE)
    else return(!pObject@isDeep)
}

is.FLSelectFrom <- function(pObj){
  if(class(pObj)=="FLSelectFrom")
  return(TRUE)
  else return(FALSE)
}

is.FL <- function(x){
    if(class(x) %in% c("FLMatrix",
                        "FLVector",
                        "FLTable",
                        "FLTableQuery",
                        "FLSelectFrom",
                        "FLTableFunctionQuery"))
    return(TRUE)
    else return(FALSE)
}

is.RSparseMatrix <- function(object){
    vsparseClass <- c("dgCMatrix","dgeMatrix","dsCMatrix",
                    "dgTMatrix","dtrMatrix","pMatrix",
                    "dspMatrix","dtCMatrix","dgRMatrix",
                    "ddiMatrix","dpoMatrix"
                    )
    if(class(object) %in% vsparseClass)
    return(TRUE)
    else
    return(FALSE)
}

is.TD <- function(){
    if(getOption("FLPlatform")=="TD")
    return(TRUE)
    else return(FALSE)
}

is.TDAster <- function(){
    if(getOption("FLPlatform")=="TDAster")
    return(TRUE)
    else return(FALSE)
}

is.Hadoop <- function(){
    if(getOption("FLPlatform")=="Hadoop")
    return(TRUE)
    else return(FALSE)
}

isDotFormula <- function(pFormula){
    if(!is.formula(pFormula))
        return(FALSE)
    vallVars <- all.vars(pFormula)
    if("."==vallVars[1])
        stop(". supported on RHS of formula currently \n ")
    else if("." %in% vallVars)
        return(TRUE)
    else return(FALSE)
}

is.formula <- function(pObject){
    return(class(pObject)=="formula")
}

is.FLTableFunctionQuery <- function(pObject)
    return(class(pObject)=="FLTableFunctionQuery")