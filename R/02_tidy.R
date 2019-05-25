#' bng tag to enable tidy evaluation or arguments
#'
#' Use tidy evaluation on all arguments
#'
#' @export
#' @examples
#' u <- "speed"
#' v <- quote(dist)
#' w <- rlang::quo(time)
#' x <- list(a=c(1, 2), b=c(3, 4))
#' bng$transform(head(cars,2), !!w := !!v / !!rlang::sym(u), !!!x)
bng <- tag({
  exprs <- do.call(rlang::exprs, ARGS)
  call <- as.call(c(f, exprs))
  eval.parent(call)})

#' lbd tag to use formula notation on FUN arguments
#'
#' inspired by `gsubfn::fn`, `..fn` is similar but use `rlang::as_function`
#' while *gsubfn* uses its own API.
#'
#' @export
#' @examples
#' lbd$ave(c(1,2,4,3,NA),c(1,1,2,2,2),FUN = ~mean(.,na.rm=TRUE))
lbd <- tag({
  if ("FUN" %in% names(ARGS)){
    call <- CALL
    call[["FUN"]] <- rlang::as_function(eval.parent(call[["FUN"]]))
  }
  eval.parent(call)
})


#' on_self tag to allow self referencing in arguments
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
#' on_self$transform(head(iris,2), Petal.Width = ~1000*(.), Species = ~toupper(.))
#' on_self$summarize(iris, Petal.Width = ~median(.), Sepal.Length = ~mean(.))
on_self <- tag({
  args <- purrr::imap(ARGS, ~if(is.call(.) && identical(.[[1]],quote(`~`))) {
    rlang::expr(purrr::as_mapper(!!.)(!!rlang::sym(.y)))
  } else .)
  call <- as.call(c(f, args))
  eval.parent(call)})



#' grp tag to group and ungroup around another operation
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
#' library(dplyr)
#' grp$summarize(iris,meanSL = mean(Sepal.Length), .by="Species")
#' grp(.by="Species")$summarize(iris,meanSL = mean(Sepal.Length))
#' grp$summarize_all(iris, mean, .by="Species")
#' grp$slice(iris,1, .by="Species")
#' # unnamed groupings are not kept
#' grp$summarize(
#'   iris,meanSW = mean(Sepal.Width), .by= vars(Species, Sepal.Width > 3.2))
#' # named groupings are kept
#' grp$summarize(
#'   iris,meanSW = mean(Sepal.Width), .by= vars(Species, long_sepal = Sepal.Width > 3.2))
grp <- tag(args = alist(.by=),{
  data <- eval.parent(ARGS[[1]])
  f_args <- as.list(match.call())
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




#' rw tag to apply a transformation rowwise
#' @export
#' @examples
#' rw$mutate(head(iris,3),X = mean(c(Sepal.Length, Sepal.Width)))
#' mutate(head(iris,3),X = mean(c(Sepal.Length, Sepal.Width)))
rw <- tag({
  CALL[[2]] <- bquote(dplyr::rowwise(.(CALL[[2]])))
  eval.parent(bquote(dplyr::ungroup(.(CALL))))
})
