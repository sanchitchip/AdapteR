NULL


setOldClass("RODBC")



#' A table query models a select or a table result of a sql statement.
#' 
#' 
#' @slot connection ANY ODBC/JDBC connectivity for R
#' @slot variables list Named list of variables for the table query: values are rownames, names (keys) are result column names.
#' @slot whereconditions character vector of strings restricting the select query (if any)
#' @slot order character ordering statements (if any)
#' @export
setClass("FLTableQuery",
         slots=list(
             connection = "ANY",
             variables="list",
             whereconditions="character",
             order = "character"
         ))



##' A selectFrom models a select from a table.
##'
##' @slot table_name character the name of the table to select from (possibly fully qualified, i.e. with database)
##' @export
setClass("FLSelectFrom",
         contains="FLTableQuery",
         slots=list(
             table_name = "character"
         ))

setClass("FLAbstractTable",
    slots = list(
        select = "FLTableQuery",
        dimnames = "list",
        isDeep = "logical",
        mapSelect = "FLSelectFrom"
    )
)

setClass("FLAbstractColumn",
	slots=list(
            columnName = "character"))

##' A TableFunctionQuery models a select from an arbitrary query
##'
##' @slot SQLquery character The free SQL query returning a table.
##' @export
setClass("FLTableFunctionQuery",
         contains="FLTableQuery",
         slots=list(
             SQLquery = "character"
         ))

##' An S4 class to represent FLMatrix.
##' A Matrix can be either based off a query from a deep table (customizable by any where-condition)
##' -- or based off an arbitrary SQL statement returning a deep table.
##'
##' @slot dimnames list of 2 elements with row, column names of FLMatrix object
##' @slot dim list of 2 FLTableQuery instances (or NULL) that map row_ids in the select to row-names in R
##' @export
setClass("FLMatrix",
         slots = list(
             select = "FLTableQuery",
             mapSelect  = "FLSelectFrom",
             dimColumns = "character",
             type       = "character",
             dim = "ANY",
             dimnames = "ANY"
         ),prototype = prototype(dimColumns=c("rowIdColumn","colIdColumn","valueColumn"),
                                 type="double")
         )

#' An S4 class to represent FLTable, an in-database data.frame.
#'
#' @slot select FLTableQuery the select statement for the table.
#' @slot dimnames the observation id and column names
#' @slot isDeep logical (currently ignored)
#' @method names FLTable
#' @param object retrieves the column names of FLTable object
#' @export
setClass("FLTable",
         slots = list(
             select = "FLTableQuery",
             dimnames = "list",
             isDeep = "logical",
             mapSelect = "FLSelectFrom",
             type       = "character"
         ),prototype = prototype(type="double")
        )

#' An S4 class to represent FLTableMD, an in-database data.frame.
#'
#' @slot select FLTableQuery the select statement for the table.
#' @slot dimnames the observation id and column names
#' @slot isDeep logical (currently ignored)
#' @slot mapSelect \code{FLSelectFrom} object which contains the 
#' mapping information if any
#' @export
setClass(
    "FLTableMD",
    contains="FLTable",
    slots = list(
        select = "FLTableQuery",
        dimnames = "list",
        isDeep = "logical",
        mapSelect = "FLSelectFrom"
    )
)

#' An S4 class to represent FLVector
#'
#' @export
setClass("FLVector",
         slots = list(
             select = "FLTableQuery",
             dimnames = "list",
             isDeep= "logical",
             mapSelect = "FLSelectFrom",
             type       = "character"
         ),prototype = prototype(type="double")
        )

setMethod("str",signature(object="FLVector"),
          function(object) cat(paste0("FLVector [",length(object),"] ", object@type, "\n   ",str(object@select),"\n")))
setMethod("str",signature(object="FLMatrix"),
          function(object) cat(paste0("FLMatrix [",nrow(object),",",ncol(object),"] ",
                                      object@type, "\n   ",object@type, str(object@select),"\n")))
setMethod("str",signature(object="FLTable"),
          function(object) cat(paste0("'FLTable': ",nrow(object)," obs. of ",ncol(object)," variables:",
                                      paste0(" $ ",names(object@type)," : ",object@type, collapse="\n "),
                                      "\n  ", str(object@select),
                                      "\n"
                                      )))
setMethod("str",signature(object="FLTableQuery"),
          function(object) gsub("[ \n]+"," ",constructSelect(object)))

##' drop a table
##' 
##' @param object FLTable object 
##' @return message if the table is dropped
##' @export
drop.FLTable <- function(object)
{
    vSqlStr <- paste0(" DROP TABLE ",object@tablename)
    sqlSendUpdate(getConnection(object), vSqlStr)
    return(paste0(object@select@table_name," DROPPED"))
}



setGeneric("getVariables", function(object) {
    standardGeneric("getVariables")
})
setMethod("getVariables",
          signature(object = "FLTableQuery"),
          function(object) {
              variables <- object@variables
              if(length(variables)==0) return("*")
              if(is.null(names(variables)))
                  names(variables) <- variables
              else
                  names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]
              names(variables) <- gsub("\\*","",names(variables))
              return(variables)
          })
setMethod("getVariables",
          signature(object = "FLMatrix"),
          function(object) getVariables(object@select))
setMethod("getVariables",
          signature(object = "FLTable"),
          function(object) getVariables(object@select))
setMethod("getVariables",
          signature(object = "FLTableMD"),
          function(object) getVariables(object@select))
setMethod("getVariables",
          signature(object = "FLVector"),
          function(object) getVariables(object@select))



setGeneric("suffixAlias", function(object,suffix,...) {
    standardGeneric("suffixAlias")
})

setMethod("suffixAlias",
          signature(object = "FLMatrix",suffix="character"),
          function(object,suffix){
              object@select <- suffixAlias(object@select,suffix)
              object@mapSelect <- suffixAlias(object@mapSelect,suffix)
              return(object)
          })
setMethod("suffixAlias",
          signature(object = "FLSelectFrom",suffix="character"),
          function(object,suffix){
              for(talias in names(object@table_name)){
                  object@variables <- llply(
                      object@variables,
                      function(v) gsub(paste0("^",talias,"\\."),
                                       paste0(talias,suffix,"."),
                                       v))
                  object@whereconditions <- gsub(paste0("^",talias,"\\."),
                                       paste0(talias,suffix,"."),
                                       object@whereconditions)
              }
              if(!is.null(names(object@table_name)))
                  names(object@table_name) <- paste0(names(object@table_name),suffix)
              return(object)
          })

renameDuplicates <- function(vec)
{
    for(x in vec)
    {
        id <- which(vec %in% x)
        if(length(id)>1)
            vec[id] <- paste0(vec[id],1:length(id))
    }
    return(vec)
}

FLNamesMappedP <- function(m) return(0<length(m@mapSelect@table_name))

getConditionValues <- function(selection,names) FLNameMatch(selection,names)

FLName <- function(i,thenames){
    ##browser()
    if(is.null(i))
        return(thenames)
    if(is.null(thenames)){
        names(i) <- i
        return(i)
    }
    if(is.numeric(i) & is.null(names(i))){
        RR <- thenames[i]
        if(is.null(names(RR)) & !is.null(names(thenames)))
            names(RR) <- names(thenames)[i]
        return(RR)
    }
    i <- thenames[match(i,thenames)]
    return(i)
}

FLNameMatch <- function(i,thenames){
    ##browser()
    if(is.null(thenames)){
        return(i)
    }
    if(is.numeric(i)){
        if(is.null(names(thenames))){
            return(thenames[i])
        } else {
            return(names(thenames)[i])
        } 
    }
    if(!is.null(names(thenames)))
        return(names(thenames)[match(i,thenames)])
    else
        return(i)
}

FLIndexOf <- function(i,thenames){
    if(is.null(thenames))
        return(i)
    if(!is.null(names(thenames))) ##FLindex?
        return(match(i,names(thenames)))
    else {
        if(is.numeric(i))
            return(i)
        else
            return(match(i,thenames))
    }
}

#' Appends where clauses for subsetting etc.
#'
#' @export
#' @param object An FLMatrix object
#' @param whereconditions constraints to be added
#' @param dimnames new dimension names
#' @param conditionDims vector of 2 LOGICAL values, if first is TRUE,
#' a inCondition for the rownames is appended, if 2 for the columns respectively.
#' @export
restrictFLMatrix <-
    function(object,
             whereconditions = object@select@whereconditions,
             dimnames = object@dimnames,
             conditionDims = c(FALSE,FALSE)){
        ##browser()
        if(is.null(dimnames)) return(object)
        if(is.FLTableFunctionQuery(object@select))
          object <- store(object)
        ## if there is a mapping table, use indices instead of dimnames
        vars <- object@select@variables[object@dimColumns]
        for(i in 1:2)
            if(conditionDims[[i]])
                whereconditions <-
                c(whereconditions,
                  inCondition(
                      vars[[i]],
                      getConditionValues(
                          dimnames[[i]],
                          object@dimnames[[i]])))

        object@select@whereconditions <-
            unique(c(object@select@whereconditions,
                     whereconditions))
        for(i in 1:2)
            if(!is.null(dimnames[[i]])){
                object@dim[[i]] <- length(dimnames[[i]])
                object@dimnames[[i]] <- FLName(dimnames[[i]],
                                               object@dimnames[[i]])
            }

        object
    }

##' Amends dimension names to a matrix if a mapping exists.  
##' 
##' If no mapping exists uses the unique row and column names.
##' Dimnames 1:n are set to NULL in order to adhere to R conventions.
##'
##' @param flm FLMatrix 
##' @param map_table name of the mapping table if already exists
##' @return the FLMatrix object, with slot dimnames re set 
#' @export
FLamendDimnames <- function(flm,map_table) {
    ##browser()
    checkNames <- function(colnames, addIndex=FALSE){
        if(is.numeric(colnames) && colnames==1:length(colnames))
            colnames <- c()
        if(length(colnames)==0) colnames <- NULL
        if(addIndex & !is.null(colnames) & is.null(names(colnames)))
            names(colnames) <- 1:length(colnames)
        return(colnames)
    }
    connection <- getConnection()
    dimnames <- flm@dimnames
    ##print(dimnames)
    if(is.null(dimnames) & !is.null(map_table)){
        ## if there is currently no dimnames set,
        ## receive dimnames from map table
        ##browser()
        mapSelect <- flm@select
        mapSelect@variables <- list()
        mapSelect@table_name <- map_table
        names(mapSelect@table_name) <- names(flm@select@table_name) ## todo: generalize
        mapSelect@order <- c("DIM_ID","NUM_ID")
        mapSQL <- constructSelect(mapSelect)
        if(!is.null(mapSQL)){
            mapDF <- sqlQuery(connection, mapSQL)
            if(nrow(mapDF)>0){
                rownames <- mapDF[mapDF$DIM_ID==1,"NAME"]
                names(rownames) <- mapDF[mapDF$DIM_ID==1,"NUM_ID"]
                colnames <- mapDF[mapDF$DIM_ID==2,"NAME"]
                names(colnames) <- mapDF[mapDF$DIM_ID==2,"NUM_ID"]
            }
        } else
            rownames <- colnames <- c()
    } else {
        rownames <- dimnames[[1]]
        colnames <- dimnames[[2]]
    }
    
    ## if there is still no dimnames set,
    ## use unique values as dimnames
    selectUnique <- function(varname)
        paste0("SELECT DISTINCT(",
               flm@select@variables[[varname]],
               ") as v\n",
               "FROM  ",tableAndAlias(flm@select),
               constructWhere(
                   constraintsSQL(flm@select)),
               "\nORDER BY 1")
    vrownames <- sqlQuery(connection,selectUnique("rowIdColumn"))
    vcolnames <- sqlQuery(connection,selectUnique("colIdColumn"))
    if(length(rownames)==0)
        rownames <- sort(vrownames$v)
    if(length(colnames)==0)
        colnames <- sort(vcolnames$v)

        vstringdimnames <- lapply(list(rownames,colnames),
                                  function(x){
                                      if(is.factor(x))
                                      as.character(x)
                                      else x
                                  })
        rownames <- vstringdimnames[[1]]
        colnames <- vstringdimnames[[2]]
    if(all(flm@dim==0))
        flm@dim <- c(length(rownames),length(colnames))
    
    dimnames <- flm@dimnames <- list(checkNames(rownames),
                                     checkNames(colnames))


    if(!is.null(map_table)){
        mConstraint <- c()
        tablenames <- c()
        variables <- list()
        names(map_table) <- NULL
        ## row name maps
        if(!is.null(dimnames[[1]])){
            tablenames <- c(tablenames,rnmap=map_table)
               variables$rowNameColumn <- "rnmap.NAME"
            mConstraint <-
                c(mConstraint,
                  gsub("mtrx","rnmap", flm@select@whereconditions),
                  equalityConstraint(
                      paste0(flm@select@variables[[flm@dimColumns[[1]]]]),
                      "rnmap.NUM_ID"),
                  equalityConstraint("rnmap.DIM_ID","1"))
        }

        ## column name maps
        if(!is.null(dimnames[[2]])){
            tablenames <- c(tablenames,cnmap=map_table)
            variables$colNameColumn <- "cnmap.NAME"
            mConstraint <-
                c(mConstraint,
                  gsub("mtrx","cnmap", flm@select@whereconditions),
                  equalityConstraint(
                      paste0(flm@select@variables[[flm@dimColumns[[2]]]]),
                      "cnmap.NUM_ID"),
                  equalityConstraint("cnmap.DIM_ID","2"))
        }
        flm@mapSelect <- new("FLSelectFrom",
                             connection = connection,
                             table_name = tablenames,
                             variables=variables,
                             whereconditions=mConstraint,
                             order = "")
    }

    return(flm)
}

#' Constructor function for FLMatrix.
#'
#' \code{FLMatrix} constructs an object of class \code{FLMatrix}.
#'
#' \code{FLMatrix} object is an in-database equivalent to matrix object.
#' This object is used as input for matrix operation functions.
#' @param connection ODBC/JDBC connection handle to the database.
#' @param database name of the database
#' @param table_name name of the matrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id_colname matrix id value in \code{table_name}
#' @param row_id_colname column name in \code{table_name} where row numbers are stored
#' @param col_id_colname column name in \code{table_name} where column numbers are stored
#' @param cell_val_colname column name in \code{table_name} where matrix elements are stored
#' @param dim vector representing the dimensions of the matrix
#' @param dimnames list of dimension names to assign to the matrix
#' @param conditionDims logical vector of length two representing if there are any conditions on dimensions
#' @param whereconditions where conditions if any to reference the in-database matrix
#' @param map_table in-database table name which stores the dimnames of the matrix. Not required if dimnames are already specified using \code{dimnames}
#' @return \code{FLMatrix} returns an object of class FLMatrix mapped
#' to an in-database matrix.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti",
#'                      5, "Matrix_id","ROW_ID","COL_ID","CELL_VAL")
#' flmatrix
#' @export
FLMatrix <- function(table_name,
                     matrix_id_value = "",
                     matrix_id_colname = "",
                     row_id_colname = "rowIdColumn",
                     col_id_colname = "colIdColumn",
                     cell_val_colname = "valueColumn",
                     dim=0,
                     dimnames = NULL,
                     conditionDims=c(FALSE,FALSE),
                     whereconditions=c(""),
                     map_table=NULL,
                     connection=getOption("connectionFL"),
                     type="double"){
  ## If alias already exists, change it to flt.
    if(length(names(table_name))>0)
    oldalias <- names(table_name)[1]
    else oldalias <- ""
    matrix_id_colname <- changeAlias(matrix_id_colname,"mtrx",oldalias)
    row_id_colname <- changeAlias(row_id_colname,"mtrx",oldalias)
    col_id_colname <- changeAlias(col_id_colname,"mtrx",oldalias)
    cell_val_colname <- changeAlias(cell_val_colname,"mtrx",oldalias)
    whereconditions <- changeAlias(whereconditions,
                                  "mtrx",
                                  c(getTablename(table_name),
                                    table_name,
                                    oldalias))
    names(table_name) <- "mtrx"

    mConstraint <-
        equalityConstraint(
            paste0(matrix_id_colname),
            matrix_id_value)
    tablenames <- table_name
    variables <- list(
        MATRIX_ID="'%insertIDhere%'",
        rowIdColumn=paste0(row_id_colname),
        colIdColumn=paste0(col_id_colname),
        valueColumn=paste0(cell_val_colname))
    
    checkNames <- function(colnames){
        if(is.numeric(colnames) && colnames==1:length(colnames))
          return(NULL)
        else return(colnames)
    }

    dimnames <- list(checkNames(dimnames[[1]]),
                    checkNames(dimnames[[2]]))

      if((length(dimnames[[1]])>0 || 
        length(dimnames[[2]])>0 )&& 
        length(map_table)==0)
      {
        remoteTable <- getOption("NameMapTableFL")

        t<-sqlSendUpdate(connection,
                      paste0(" DELETE FROM ",remoteTable,
                        " WHERE MATRIX_ID=",matrix_id_value))
        
        map_table <- getOption("NameMapTableFL")
        for(i in 1:length(dimnames))
            #if(is.character(mydimnames[[i]]))
            {
                map_table <- getOption("NameMapTableFL")
                if(!is.null(dimnames[[i]]))
                dimnames[[i]] <- storeVarnameMapping(
                    connection,
                    map_table,
                    matrix_id_value,
                    i,
                    dimnames[[i]])
            }
        }

    ##browser()
    select <- new("FLSelectFrom",
                  connection = connection,
                  table_name = tablenames,
                  variables=variables,
                  whereconditions=c(whereconditions, mConstraint),
                  order = "")
    
    RESULT <- new("FLMatrix",
                  select = select,
                  dim = dim,
                  dimnames = dimnames,
                  type=type)
    
    RESULT <- FLamendDimnames(RESULT,map_table)

    RESULT <- restrictFLMatrix(RESULT,
                               whereconditions,
                               dimnames,
                               conditionDims)

    if(""!=matrix_id_value && ""!=matrix_id_colname){
        select@variables$matrixId <- matrix_id_colname
    }
    return(RESULT)
}


inCondition <- function(col,vals){
    if(length(vals)>0)
        paste0(col," IN (", paste0("'",vals,"'",collapse= ", "), ")")
    else
        ""
}

equalityConstraint <- function(tableColName,constantValue){
    if(""==constantValue)
        ""
    else
        paste0(tableColName, "=",constantValue)
}


##' @export
setGeneric("getConnection", function(object) {
    standardGeneric("getConnection")
})
setMethod("getConnection", signature(object = "ANY"), function(object) getOption("connectionFL"))
setMethod("getConnection", signature(object = "FLMatrix"),
          function(object) object@select@connection)
setMethod("getConnection", signature(object = "FLTable"),
          function(object) object@select@connection)
setMethod("getConnection", signature(object = "FLTableQuery"),
          function(object) object@select@connection)
setMethod("getConnection", signature(object = "FLVector"),
          function(object) object@select@connection)

setGeneric("constraintsSQL", function(object) {
    standardGeneric("constraintsSQL")
})
setMethod("constraintsSQL", signature(object = "FLMatrix"),
          function(object) {
              return(constraintsSQL(object@select))
          })
setMethod("constraintsSQL", signature(object = "FLTable"),
          function(object) {
              return(constraintsSQL(object@select))
          })
setMethod("constraintsSQL", signature(object = "FLVector"),
          function(object) {
              return(constraintsSQL(object@select))
          })
setMethod("constraintsSQL", signature(object = "FLSelectFrom"),
          function(object) {
              constraints <- object@whereconditions
              tablenames <- object@table_name
              if(!is.null(names(tablenames)))
                  for(ti in 1:length(tablenames)){
                      tname <- tablenames[ti]
                      talias <- names(tablenames)[ti]
                      constraints <- gsub(paste0("[^ ]*",tname,"\\."),
                                          paste0(" ",talias,"."),
                                          constraints)
                  }
              return(constraints)
          })

#' reference in-database object
#' 
#' @param object in-database object
#' @param table table name. Applicable only if object is the database name.
#' @return character vector giving reference to in-database object
setGeneric("tableAndAlias", function(object) {
    standardGeneric("tableAndAlias")
})
setMethod("tableAndAlias", signature(object = "FLMatrix"),
          function(object)
              tableAndAlias(object@select))
setMethod("tableAndAlias", signature(object = "FLTable"),
          function(object)
              tableAndAlias(object@select))
setMethod("tableAndAlias", signature(object = "FLVector"),
          function(object)
              tableAndAlias(object@select))
setMethod("tableAndAlias", signature(object = "character"),
          function(object){
             if(is.null(names(object))||
                names(object)=="")
                return(paste0(sapply(1:length(object),
                            function(x)
                            paste0(object[x])),
                                  collapse=",\n  "))
              else
                return(paste0(sapply(1:length(object),
                            function(x)
                            paste0(object[x],
                                  " AS ",names(object)[x])),
                            collapse=",\n  "))
          })
setMethod("tableAndAlias", signature(object = "FLSelectFrom"),
          function(object){
    return(tableAndAlias(object@table_name))
          })

#' Compare Matrix Dimensions
#'
#' Takes two matrices in-database or R, and returns true if they have same dimensions
#' @param object1 FLMatrix or R Matrix
#' @param object2 FLMatrix or R Matrix
#' @return logical
#' @export
setGeneric("checkSameDims", function(object1,object2) {
    standardGeneric("checkSameDims")
})
setMethod("checkSameDims", signature(object1="FLMatrix",object2="FLMatrix"),
          function(object1,object2) {
              if(!((nrow(object1)==nrow(object2))&&(ncol(object1)==ncol(object2))))
                  return(stop("ERROR: Invalid matrix dimensions for Operation"))
          })
setMethod("checkSameDims", signature(object1="FLMatrix",object2="matrix"),
          function(object1,object2) {
              if(!((nrow(object1)==nrow(object2))&&(ncol(object1)==ncol(object2))))
                  return(stop("ERROR: Invalid matrix dimensions for Operation"))
          })
setMethod("checkSameDims", signature(object1="FLMatrix",object2="Matrix"),
          function(object1,object2) {
              if(!((nrow(object1)==nrow(object2))&&(ncol(object1)==ncol(object2))))
                  return(stop("ERROR: Invalid matrix dimensions for Operation"))
          })
setMethod("checkSameDims", signature(object1="FLMatrix",object2="dgCMatrix"),
          function(object1,object2) {
              if(!((nrow(object1)==nrow(object2))&&(ncol(object1)==ncol(object2))))
                  return(stop("ERROR: Invalid matrix dimensions for Operation"))
          })

## todo: create FLMatrix with rounded values (FLRound)
setMethod("round","FLMatrix",function(x, digits=0) round(as.matrix(x),digits))

#' @export
print.FLMatrix <- function(object)
{
    print(as.matrix(object,sparse=TRUE))
}

#' @export
setMethod("show","FLMatrix",print.FLMatrix)

#' Check if sql query limits have been violated
#'
#' Limits can be on length of query to be parsed
#' or number of nested queries
#' @param pObj1 input FL object
#' @return logical TRUE if limits violated
setGeneric("checkQueryLimits", function(pObj1) {
    standardGeneric("checkQueryLimits")
})
setMethod("checkQueryLimits",
          signature(pObj1="character"),
          function(pObj1) {
              return(object.size(pObj1)>999000 # 1Kb tolerance
                    ||length(gregexpr("FROM",
                                      pObj1)[[1]])>120) ## Actual 140
          })
setMethod("checkQueryLimits",
          signature(pObj1="FLMatrix"),
          function(pObj1) checkQueryLimits(constructSelect(pObj1)))
setMethod("checkQueryLimits",
          signature(pObj1="FLVector"),
          function(pObj1) checkQueryLimits(constructSelect(pObj1)))
setMethod("checkQueryLimits",
          signature(pObj1="FLTable"),
          function(pObj1) checkQueryLimits(constructSelect(pObj1)))
setMethod("checkQueryLimits",
          signature(pObj1="ANY"),
          function(pObj1) return(FALSE))

`dimnames<-.FLMatrix` <- function(x,value){
  lapply(1:2,function(i){
    if(length(value[[i]])!=dim(x)[i] && !is.null(value[[i]])
      && !is.null(dim(x)[i]))
      stop("length mismatch between dimnames and dim \n ")
    })
  vdimnames <- lapply(value,
                    function(i){
                      if(is.null(i))
                        return(i)
                      if(all(i==1:length(i)))
                      return(NULL)
                      vnames <- i
                      names(vnames)<-1:length(i)
                      return(vnames)
                    })
  x@dimnames <- vdimnames
  x@mapSelect <- new("FLSelectFrom")
  x
}

`rownames<-.FLMatrix` <- function(x,value){
  if(length(value)!=dim(x)[1] && !is.null(value)
    && !is.null(dim(x)[1]))
    stop("length mismatch between dimnames and dim \n ")
  if(is.null(value) || all(value==1:length(value)))
    vnames <- NULL
  else vnames <- value
  names(vnames)<-1:length(value)
  x@dimnames <- list(vnames,x@dimnames[[2]])
  x@mapSelect <- new("FLSelectFrom")
  x
}

`colnames<-.FLMatrix` <- function(x,value){
  if(length(value)!=dim(x)[2] && !is.null(value)
    && !is.null(dim(x)[2]))
    stop("length mismatch between dimnames and dim \n ")
  if(is.null(value) || all(value==1:length(value)))
    vnames <- NULL
  else vnames <- value
  names(vnames)<-1:length(value)
  x@dimnames <- list(x@dimnames[[1]],vnames)
  x@mapSelect <- new("FLSelectFrom")
  x
}
