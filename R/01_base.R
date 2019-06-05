# 3 functions

#' preserving_attr tag to preserve attributes
#' @param .arg index or name of argument to preserve
#' @param .incl_row.names wether to include `row.names` in attributes to preserve
#' @param .incl_class wether to include `class` in attributes to preserve
#' @param .incl_names wether to include `names` in attributes to preserve
#' @param .incl_dim wether to include `dim` in attributes to preserve
#' @export
#' @examples
#' preserving_attr(.incl_class = TRUE)(purrr::map_dfr)(head(iris,2),identity)
#' preserving_attr(.incl_class = TRUE, .incl_row.names = TRUE)$lapply(head(iris,2),identity)
preserving_attr <- tag(args = alist(
  .arg = 1,
  .incl_row.names = FALSE,
  .incl_class = FALSE,
  .incl_names = FALSE,
  .incl_dim = FALSE), {
    attr_saved <- attributes(eval.parent(F_ARGS(eval = FALSE)[[.arg]]))
    attr_saved[names(attr_saved) %in% c(
      "row.names"[!.incl_row.names],
      "class"[!.incl_class],
      "dim"[!.incl_dim])] <- NULL
    res <- CALL(eval = TRUE)
    attributes(res)[names(attr_saved)] <- attr_saved
    res
  })

#' negating tag to negate the input function
#'
#' tag counterpart to `base::Negate`, and a wrapper around it.
#'
#' @export
#' @examples
#' negating$is.character(1)
#' negating$is.numeric(1)
negating <- tag({
  call <- CALL(eval = FALSE)
  call[[1]] <- Negate(f)
  eval.parent(call)
})

#' vectorizing tag to vectorize the input function
#'
#' tag counterpart to `base::Vectorize`, and a wrapper around it.
#'
#' @param .vectorize.args a character vector of arguments which should be
#'   vectorized. Defaults to all arguments of FUN.
#' @param .SIMPLIFY logical or character string; attempt to reduce the result
#'   to a vector, matrix or higher dimensional array; see the `simplify`
#'   argument of `sapply`.
#' @param .USE.NAMES logical; use names if the first ... argument has names, or
#'   if it is a character vector, use that character vector as the names.
#' @export
#' @examples
#' Vectorize(rep.int)(1:4, 4:1)
#' vectorizing$rep.int(1:4, 4:1)
vectorizing <- tag(args = alist(
  .vectorize.args = NULL,
  .SIMPLIFY = TRUE,
  .USE.NAMES = TRUE),{
    if(is.null(.vectorize.args))  {
      arg.names <- as.list(formals(f))
      arg.names[["..."]] <- NULL
      arg.names <- names(arg.names)
      .vectorize.args <- arg.names
    }
    call <- CALL(eval = FALSE)
    call[[1]] <- Vectorize(f, .vectorize.args, .SIMPLIFY, .USE.NAMES)
    eval.parent(call)
  })
