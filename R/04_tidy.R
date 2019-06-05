# 5 functions

#' using_bang tag to enable tidy evaluation or arguments
#'
#' Use tidy evaluation on all arguments
#'
#' @export
#' @examples
#' u <- "speed"
#' v <- quote(dist)
#' w <- rlang::quo(time)
#' x <- list(a=c(1, 2), b=c(3, 4))
#' using_bang$transform(head(cars,2), !!w := !!v / !!rlang::sym(u), !!!x)
using_bang <- tag({
  exprs <- eval.parent(bquote(do.call(rlang::exprs, .(F_ARGS(eval = FALSE, type = "raw")))))
  #exprs <- do.call(rlang::exprs, F_ARGS(eval = FALSE, type = "raw"))
  call <- as.call(c(f, exprs))
  eval.parent(call)})

#' using_lambda tag to use formula notation on FUN arguments
#'
#' inspired by `gsubfn::fn`, `..fn` is similar but use `rlang::as_function`
#' while *gsubfn* uses its own API.
#'
#' @export
#' @examples
#' using_lambda$ave(c(1,2,4,3,NA),c(1,1,2,2,2),FUN = ~mean(.,na.rm=TRUE))
using_lambda <- tag({
  if ("FUN" %in% names(F_ARGS(eval = FALSE))){
    call <- CALL(eval = FALSE)
    call[["FUN"]] <- rlang::as_function(eval.parent(call[["FUN"]]))
  }
  eval.parent(call)
})

#' self_referencing tag to allow self referencing in arguments
#'
#' Enhance functions like `transform`, `mutate`, `summarize` ... by allowing
#' arguments to reference themselves when they are formulas.
#'
#' to detect rigorously the type of the argument it would be necessary to
#' evaluate it, which for these functions is not an option, therefore we
#' just check if the expressions starts with a `~` character.
#'
#' @export
#'
#' @examples
#' self_referencing$transform(head(iris,2), Petal.Width = ~1000*(.), Species = ~toupper(.))
#' \dontrun{
#' library(dplyr)
#' self_referencing$summarize(iris, Petal.Width = ~median(.), Sepal.Length = ~mean(.))
#' }
self_referencing <- tag({
  args <- purrr::imap(F_ARGS(eval = FALSE), ~if(is.call(.) && identical(.[[1]],quote(`~`))) {
    rlang::expr(purrr::as_mapper(!!.)(!!rlang::sym(.y)))
  } else .)
  call <- as.call(c(f, args))
  eval.parent(call)})



#' grouping_by tag to group and ungroup around another operation
#'
#' The tagged function gains a parameter `.by`, groups that existed before
#' are preserved. One can group by a computed variable, it will be kept if
#' named, dropped otherwise.
#'
#' @param .by variables to group by, either as a character vector or a call to
#'   `vars`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' grouping_by$summarize(iris,meanSL = mean(Sepal.Length), .by="Species")
#' grouping_by(.by="Species")$summarize(iris,meanSL = mean(Sepal.Length))
#' grouping_by$summarize_all(iris, mean, .by="Species")
#' grouping_by$slice(iris,1, .by="Species")
#' # unnamed groupings are not kept
#' grouping_by$summarize(
#'   iris,meanSW = mean(Sepal.Width), .by= vars(Species, Sepal.Width > 3.2))
#' # named groupings are kept
#' grouping_by$summarize(
#'   iris,meanSW = mean(Sepal.Width), .by= vars(Species, long_sepal = Sepal.Width > 3.2))
#'   }
grouping_by <- tag(args = alist(.by=),{
  #data <- eval.parent(F_ARGS()[[1]])
  f_args <- as.list(match.call())
  data <- eval.parent(f_args[[2]])
  f_args[1:2]     <- NULL # function and data
  f_args[[".by"]] <- NULL

  tmp_by_cols <- if (inherits(.by, "quosures")) {
    nms  <- rlang::names2(.by)
    vals <- purrr::map_chr(.by[nms == ""],~deparse(.[[2]]))
    setdiff(vals, names(data))
  } else character(0)

  if (is.character(.by))
    .by <- tags:::tbl_at_syms(data, .by)
  if (is.null(.by)) {
    x <- eval(rlang::expr(f(data, !!!f_args)))
  } else  {
    # save existing groups
    groups <- dplyr::group_vars(data)
    # add new groups
    x <- dplyr::group_by(data,!!!.by, add = TRUE)
    # call the former function
    x <- eval(rlang::expr(f(x, !!!f_args)))
    # re establish former groups
    x <- dplyr::group_by(x, !!!rlang::syms(groups), add = FALSE)
    # remove temporary by cols
    if (length(tmp_by_cols)) x <- dplyr::select(x,-!!rlang::sym(tmp_by_cols))
  }
  x
})




#' using_rowwise tag to apply a transformation rowwise
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' using_rowwise$mutate(head(iris,3),X = mean(c(Sepal.Length, Sepal.Width)))
#' mutate(head(iris,3),X = mean(c(Sepal.Length, Sepal.Width)))
#' }
using_rowwise <- tag({
  call <- CALL(eval = FALSE)
  call[[2]] <- bquote(dplyr::rowwise(.(call[[2]])))
  eval.parent(bquote(dplyr::ungroup(.(call))))
})
