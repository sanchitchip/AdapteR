glm1 <- function (formula,data=list(),...) {
    UseMethod("glm1", data)
}



#' @export
glm1.default <- function (formula,data=list(),...) {
    return(stats::glm(formula,data=data,...))
}

#'fltbl <- FLTable("tblDrugWide", "OBSID")
glm1.FLTable <- function(formula,data=list(),...){
    browser()
    if(isDeep(data)){
        stop("only works for wide-table as of now")
    }
    vformula <- formula
    vcall <- match.call()
    
    
    
    
}
