# 1 function

#' checking_args to test arguments before running a call
#'
#' checking_args can make checks more readable and avoid defining temporary
#' variables
#' @param .check an expression that should be `TRUE` for the call to proceed
#' @param .message An error message, falls back on a default message if NULL
#' @export
#' @examples
#' \dontrun{
#' mean(1:5, na.rm = 1)
#' checking_args(is.logical(na.rm))$mean(1:5, na.rm = 1)
#' checking_args(is.logical(na.rm),"na.rm should be logical!")$mean(1:5, na.rm = 1)
#' }
checking_args <- tag(args = alist(.check = , .message = NULL), eval_args = FALSE, {
  t_args <- T_ARGS(eval=FALSE)
  .message <- eval.parent(t_args[[".message"]])
  if (is.null(.message)) .message <- paste(deparse(t_args[[".check"]]),"is not TRUE")
  if (!with(F_ARGS(eval=TRUE), eval(t_args[[".check"]]))) stop(.message)
  CALL(eval = TRUE)
})
