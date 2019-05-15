#' logging tag to log call and time it took
#'
#' @param .print logical, wether to print the object
#' @param .time logical, wether to print the time
#' @export
#' @examples
#' logging$head(cars,2)
logging <- tag(args = alist(.time = TRUE, .print = TRUE),{
  message(deparse(match.call()))
  cat("  ~ ...")
  if(.time) cat("\b\b\b", system.time(res <- eval(CALL))[3], "sec\n") else
    res <- eval(CALL)
  if(.print) print(res)
  invisible(res)
})


#' strictly tag to adjust strictness of a call
#'
#' @param warn numeric, forwarded to `options()`
#' @export
#' @examples
#' try(strictly$sqrt(-1))
#' strictly(0)$sqrt(-1)
#' strictly(-1)$sqrt(-1)
strictly <- tag(args = alist(warn=2),{
  w <- options("warn")[[1]]
  on.exit(options(warn=w))
  options(warn= warn)
  eval(CALL)
})


#' dbg tag to debug a call
#' @export
dbg <- tag({
  debugonce(f)
  exec("f", !!!purrr::map(ARGS, eval))
})

#' viewing tag to view the output of a call
#' @export
viewing <- tag(args = alist(.title = "View"),{
  res <- eval(CALL)
  View(res,title = .title)
  res
})

#' trying tag to try a call or if failure call alternate call
#'
#' The expression stored in `else` will be executed if
#'
#' @export
#'
#' @examples
#' trying$paste("hello","world", .else = "hi")
#' trying$paste("hello", world, .else = "hi")
#' trying$paste("hello", world, .else = "hi", .silent = TRUE)
trying <- tag(
  args = alist(.else=, .silent = FALSE),{
    res <- try(eval(CALL), silent = .silent)
    if(inherits(res, "try-error")){
      res <- .else
      if(rlang::inherits_any(res, c("function","formula")))
        res <- rlang::as_function(res)(eval(ARGS[[1]]))
    }
    res
  })

#' preserving_attr tag to preserve attributes
#' @export
#' @examples
#' preserving_attr$map_dfr(head(iris,2),identity)
#' preserving_attr(incl_class = TRUE)$map_dfr(head(iris,2),identity)
preserving_attr <- tag(args = alist(
  .arg = 1, incl_row.names = FALSE, incl_class = FALSE,
  incl_names = FALSE, incl_dim = FALSE), {
    #browser()
    eval(expr(attr_saved <- attributes(!!(ARGS[[.arg]]))))
    attr_saved[names(attr_saved) %in% c(
      "row.names"[!incl_row.names],
      "class"[!incl_class],
      "dim"[!incl_dim])] <- NULL
    res <- eval(CALL)
    attributes(res)[names(attr_saved)] <- attr_saved
    res
  })


