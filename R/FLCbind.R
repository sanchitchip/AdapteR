#' @include FLMatrix.R
NULL



#' Combine objects by columns.
#'
#' \code{cbind} combines input objects by columns and forms a FLMatrix.
#'
#' \code{cbind} takes a sequence of vector, FLVector, matrix, FLMatrix or data frames arguments,
#' combines them by columns and makes a FLMatrix.
#' @param object... can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of rows.
#' @return \code{cbind} returns a FLMatrix object which is the column-wise combination of input arguments.
#' @export
cbind.FLMatrix<-function(object,...) FLbind(list(object,...),2)

#' @export
cbind.FLMatrixBind <- cbind.FLMatrix

