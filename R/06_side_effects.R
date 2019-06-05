# 6 functions

#' logging tag to log call and time it took
#'
#' @param .print logical, wether to print the object
#' @param .time logical, wether to print the time
#' @export
#' @examples
#' logging$head(cars,2)
logging <- tag(args = alist(.time = TRUE, .print = TRUE),{
  message(deparse(sys.call()))
  if(.time) {
    time_sec <- system.time(res <- CALL(eval = TRUE))[3]
    cat("  ~", time_sec, "sec\n")
  } else {
    res <- CALL(eval = TRUE)
  }
  if(.print) print(res)
  invisible(res)
})

# #' strictly tag to adjust strictness of a call
# #'
# #' @param warn numeric, forwarded to `options()`
# #' @export
# #' @examples
# #' try(strictly$sqrt(-1))
# #' strictly(0)$sqrt(-1)
# #' strictly(-1)$sqrt(-1)
# strictly <- tag(args = alist(.warn=2),{
#   w <- options("warn")[[1]]
#   on.exit(options(warn=w))
#   options(warn= eval.parent(NEW_ARGS[[".warn"]]))
#   eval.parent(CALL)
# })

#' debugging tag to debug a call
#' @export
#' @examples
#' \dontrun{
#' debugging$mean(1:5)
#' }
debugging <- tag({
  debugonce(f)
  CALL(eval = TRUE)
})

#' viewing tag to view the output of a call
#' @param .title itle for viewer window. Defaults to `"View"`
#' @export
#' \dontrun{
#' viewing$head(iris)
#' }
viewing <- tag(args = alist(.title = "View"),{
  res <- CALL(eval = TRUE)
  View(res,title = .title)
  res
})


