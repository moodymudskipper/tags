# 2 functions

#' tracing tag
#'
#' Emulates the behavior of `base::trace`.
#'
#' It's not a wrapper around `base::trace`, instead it modifies the body of the
#' input function. If the input function is an *S3 generic* the effect of
#' the tracing can be overwritten by the argument values, better in this case
#' to use the relevant method as a function input.
#'
#' @param .tracer either a function or an unevaluated expression. The function
#'   will be called or the expression will be evaluated either at the beginning
#'   of the call, or before those steps in the call specified by the argument
#'   at.
#' @param .exit either a function or an unevaluated expression. The function
#'   will be called or the expression will be evaluated on exiting the function.
#' @param .at optional numeric vector or list. If supplied, `.tracer` will be
#' called just before the corresponding step in the body of the function.
#' @param .edit For complicated tracing, such as tracing within a loop inside
#'   the function, you will need to insert the desired calls by editing the body
#'  of the function. If so, supply the edit argument either as TRUE, or as the
#'  name of the editor you want to use. Then `tracing()` will call edit and use the
#'  version of the function after you edit it.
#' @export
#' @examples
#' tracing(quote(print("hello")), .exit = quote(print("good bye")))$mean(1:5)
tracing <- tag(args= alist(.tracer=NULL, .exit=NULL, .at=1, .edit = FALSE),{
  b <- as.list(rlang::fn_body(f))
  if (!is.null(.tracer)) b <- append(b,.tracer,.at)
  if (!is.null(.exit)) b <- append(b,bquote(on.exit(.(.exit))),1)
  body(f) <- as.call(b)
  if(.edit) f <- edit(f)
  call <- CALL(eval = FALSE)
  call[[1]] <- f
  eval.parent(call)
})

#' wrapping tag for pre/post procession and composition
#'
#' @param .before an expression to run before the call
#' @param .after an expression to run after the call, can use `.` as the result
#'   of the call
#' @param .around a function, or formula, to apply around the call before
#'   evaluation, such as `capture.output` or `SuppressWarnings`
#' @export
#' @examples
#' wrapping(.before = print("hello"),
#'           .after = print(paste("result was:", .)),
#'           .around = suppressWarnings)$sqrt(-1)
wrapping <- tag(
  args= alist(.before=NULL, .after=NULL, .around = identity),
  eval_args = FALSE,{
    t_args <- T_ARGS(eval = FALSE)
    eval(t_args[[".before"]])
    . <- eval(expr(rlang::as_function(!!t_args[[".around"]])(!!CALL(eval = FALSE))))
    eval(t_args[[".after"]])
    .
  })
