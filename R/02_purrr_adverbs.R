#' 3 functions

#' using_safely tag
#'
#' tag ounterparts of *purrr* adverbs.
#' These are not wrappers around *purrr* functions but contain some of their
#' code, and have a similar interface).
#' `trying` is an alias for `using_possibly`.
#'
#' similar to `purrr::safely`
#' @param .otherwise Default value to use when an error occurs.
#' @param .quiet Hide errors (`TRUE`, the default), or display them as they occur?
#' @export
#' @examples
#' using_safely$log(10, .otherwise = NA_real_)
#' using_safely$log("a", .otherwise = NA_real_)
#' using_safely(.otherwise = NA_real_)$log(10)
#' using_safely(.otherwise = NA_real_)$log("a")
#'
#' using_quietly$sqrt(-1)
#' using_quietly$sqrt(4)
#'
#' using_possibly$log("a", .otherwise = NA_real_)
#' using_possibly$log(10, .otherwise = NA_real_)
using_safely <- tag(args = alist(.otherwise = NULL, .quiet = TRUE),{
  tags:::capture_error(CALL(eval = TRUE), otherwise = .otherwise, quiet = .quiet)
})

#' using_quietly tag
#'
#' similar to `purrr::quietly`
#' @export
#' @rdname using_safely
using_quietly <- tag({
  eval.parent(rlang::expr(tags:::capture_output(!!CALL(eval = FALSE))))
})

#' using_possibly tag
#'
#' similar to `purrr::possibly`
#' @export
#' @rdname using_safely
using_possibly <- tag(args = alist(.otherwise=, .quiet = TRUE),{
  tryCatch(CALL(eval=TRUE), error = function(e) {
    if (!.quiet)
      message("Error: ", e$message)
    .otherwise
  }, interrupt = function(e) {
    stop("Terminated by user", call. = FALSE)
  })
})
