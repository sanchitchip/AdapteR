#' @include FLMatrix.R
NULL

#' Norm of a Matrix.
#'
#' \code{FLMatrixNorm} gives the value of Norm for FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param NormMethod is an integer from 1-4 representing the type of norm that
#' should be computed.
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (700 x 700).
#' @return \code{FLMatrixNorm} returns a R vector object which is the Norm of input
#' FLMatrix object calculated using method specified by NormMethod input.
#' There are 4 types of norms of a matrix:
#' \item{1-Norm}{Maximum of the sum of the absolute values for the columns}
#' \item{2-Norm}{Maximum of the sum of the absolute values for the rows}
#' \item{Frobenius Norm}{Square root of the trace of (t(A)A)}
#' \item{Infinity Norm}{Square root of the maximum of the magnitudes of the Eigenvalues of (t(A)A)}
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- FLMatrixNorm(flmatrix,4)
#' @export
FLMatrixNorm <- function (object,NormMethod){
	UseMethod("FLMatrixNorm", object)
}

#' @export
FLMatrixNorm.FLMatrix<-function(object,NormMethod)
{

	connection<-getConnection(object)
	flag3Check(connection)

	if(NormMethod > 4 || NormMethod < 1)
	stop("NormMethod parameter should be whole number from 1 to 4")

	sqlstr<-paste0(viewSelectMatrix(object,"a",withName="z"),
                   " SELECT a.OutputNorm 
					FROM TABLE (FLMatrixNormUdt(z.Matrix_ID,",NormMethod,", z.Row_ID, z.Col_ID, z.Cell_Val) 
					HASH BY z.Matrix_ID 
					LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;"
                   )
	sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)

	sqlstr <- ensureQuerySize(pResult=sqlstr,
		            pInput=list(object,NormMethod),
		            pOperator="FLMatrixNorm")

	return(sqlQuery(connection,sqlstr)$"OutputNorm"[1])
}
