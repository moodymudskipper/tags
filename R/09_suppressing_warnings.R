# 1 function

#' suppressing_warnings tag to Selectively suppress warnings
#'
#' `suppressing_warnings` permits the user to provide a regular expression that
#' will be forwarded to `grepl`. The warning will be suppressed in case of match.
#' If the user feeds a function or formula to `.regex` argument it will be applied
#' on the text of the warning, and the warning will be suppressed if the output
#' is `TRUE`
#'
#' @param .regex a string, a function, or a formula
#' @export
#' @examples
#' \dontrun{
#' fun <- function(x) {
#'   warning("ooops", call. = FALSE)
#'   sqrt(x)
#' }
#' fun(-1)
#' suppressing_warnings$fun(-1)
#' suppressing_warnings("prod")$fun(-1)
#' suppressing_warnings(~startsWith(., "o"))$fun(-1)
#' }
suppressing_warnings <- tag(args = alist(.regex = "*"),{
  eval.parent(substitute(
    withCallingHandlers( CALL0 , warning = function(w) {
      cm <- conditionMessage(w)
      cond <-
        if(is.character(.regex)) grepl(.regex, cm) else rlang::as_function(.regex)(cm)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    }),
    list(CALL0 = CALL(eval = FALSE), .regex=.regex)
  ))
})



