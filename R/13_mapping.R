#' mapping tag to loop on arguments
#'
#' This a wrapper around `foreach::foreach` so that a call
#' `foreach(i=1:3, .combine = c) %do% exp(i)` can be written
#' `mapping(list(x=1:3), c)$exp()`
#'
#' @inheritParams foreach::foreach
#' @param ..args a list of sequences of arguments, can be a dataframe
#' @export
#' @examples
#' mapping(list(times=1:3,x=4:6))$rep()
#' mapping(list(times=1:3))$rep(4:6)
#' mapping(list(x=1:3))$exp()
#' mapping(list(x=1:3), .combine = c)$exp()
#' area <- function(...) ..1 * ..2
#' transform(head(iris), Sepal.Area = mapping(head(iris), c)$area())
mapping <- tag(arg = alist(
  ..args=,
  .combine = rlang::missing_arg(),
  .init = rlang::missing_arg(),
  .final = NULL,
  .inorder = TRUE,
  .multicombine = FALSE,
  .maxcombine = NULL,
  .errorhandling = c("stop", "remove", "pass"),
  .packages = NULL,
  .export = NULL,
  .noexport = NULL,
  .verbose = FALSE),{
    if(!requireNamespace("foreach"))
      stop("Install the foreach package to use the tag `mapping()`")
    t_args <- T_ARGS(eval = FALSE)
    if(is.null(.maxcombine)) t_args[[".maxcombine"]] <- if (.multicombine) 100 else 2
    t_args[sapply(t_args, rlang::is_missing)] <- NULL
    fe <- do.call(foreach::foreach, c(..args, lapply(t_args[-1], eval.parent, 2)))
    call1 <- as.call(c(
      as.list(CALL(eval = FALSE)),
      rlang::set_names(syms(names(..args)))))
    eval.parent(substitute(foreach::`%do%`(FE, CALL1), list(
      FE = fe, CALL1 = call1)))
  })
