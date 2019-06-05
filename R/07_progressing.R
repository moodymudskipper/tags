# 1 function

#' progressing tag to use progress bar with functionals
#'
#' @param .format The format of the progress bar. see `?progress::progress_bar`
#' @param .width Width of the progress bar.
#' @param .complete Completion character.
#' @param .incomplete Incomplete character.
#' @param .current Current character.
#' @param .callback Callback function to call when the progress bar finishes.
#'   The progress bar object itself is passed to it as the single parameter.
#' @param .clear Whether to clear the progress bar on completion.
#' @param .show_after Amount of time in seconds, after which the progress bar is
#'   shown on the screen. For very short processes, it is probably not worth
#'   showing it at all.
#' @param .force Whether to force showing the progress bar, even if the given (or default) stream does not seem to support it.
#' @param .tokens A list of unevaluated expressions, using `alist`, to be passed
#'   passed to the `tick` method of the progress bar
#' @param .message A message to display on top of the bar
#' @param .FUN_arg name or position of function arg, by default looks for the
#'   first argument that is either `FUN`, `.f` or `f`
#' @export
#' @examples
#' \dontrun{
#' library(purrr)
#' progressing$map(1:3, ~{Sys.sleep(1);.x*2})
#' }
#' x <- progressing$lapply(1:10000, force)
#' progressing(.format = "[:bar] :eta")$lapply(1:5, function(x) {Sys.sleep(1);x*2})
progressing <- tag(args = alist(
  .format = "[:bar] :percent",
  .width = options("width")[[1]] - 2,
  .complete = "=",
  .incomplete = "-",
  .current =">",
  .callback = invisible, # doc doesn't give default but this seems to work ok
  .clear = TRUE,
  .show_after = .2,
  .force = FALSE,
  # The only arg not forwarded to progress::progress_bar$new()
  # By default `for` will self detruct after being called
  .message = NULL,
  .tokens = alist(),
  .FUN_arg=NULL),{

  # determine the position of the functional argument and evaluate it into fun
  call <- CALL(eval = FALSE)
  if (is.null(.FUN_arg))
    .FUN_arg <- min(which(names(call) %in% c("FUN",".f","f")))
  # the number of iterations is the length of the first element which isn't
  # the functional
  args <- F_ARGS(eval = FALSE)
  args[[.FUN_arg]] <- NULL
  total <- length(eval.parent(args[[1]]))


  fun <- eval(expr(as.function(c(alist(...=), quote({
    on.exit(pb$tick())
    f <- !!rlang::as_function(eval(call[[.FUN_arg]]))
    f(...)
  })))))

  call[[.FUN_arg]] <- fun

  pb <- progress::progress_bar$new(
    format = .format,
    width = .width,
    complete = .complete,
    incomplete = .incomplete,
    current = .current,
    callback = .callback,
    clear = .clear,
    show_after = .show_after,
    force = .force,
    total = total)

  if(!is.null(.message)) pb$message(.message)
  eval.parent(call)
})
