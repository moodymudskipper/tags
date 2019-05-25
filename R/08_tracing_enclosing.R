#' tracing tag
#'
#' Emulates the behavior of `base::trace`.
#'
#' It's not a wrapper around `base::trace`, instead it modifies the body of the
#' input function. If the input function is an *S3 generic* the effect of
#' the tracing can be overwritten by the argument values, better in this case
#' to use the relevant method as a function input.
#'
#' @inheritParams trace
#' @export
#' @examples
#' tracing(quote(print("hello")), exit = quote(print("good bye")))$mean(1:5)
tracing <- tag(args= alist(tracer=NULL, exit=NULL, at=1, edit = FALSE),{
  b <- as.list(rlang::fn_body(f))
  if (!is.null(tracer)) b <- append(b,tracer,at)
  if (!is.null(exit)) b <- append(b,bquote(on.exit(.(exit))),1)
  body(f) <- as.call(b)
  if(edit) f <- edit(f)
  CALL[[1]] <- f
  eval.parent(CALL)
})

#' enclosing tag for pre/post procession and composition
#'
#' @param .before an expression to run before the call
#' @param .after an expression to run after the call, can use `.` as the result
#'   of the call
#' @param .around a function, or formula, to apply around the call before
#'   evaluation, such as `capture.output` or `SuppressWarnings`
#' @export
#' @examples
#' enclosing(.before = print("hello"),
#'           .after = print(paste("result was:", .)),
#'           .around = suppressWarnings)$sqrt(-1)
enclosing <- tag(args= alist(.before=NULL, .after=NULL, .around = identity),{
  eval.parent(rlang::enexpr(.before))
  . <- eval.parent(bquote(rlang::as_function(.(rlang::enexpr(.around)))(.(CALL))))
  eval(rlang::enexpr(.after))
  .
})
