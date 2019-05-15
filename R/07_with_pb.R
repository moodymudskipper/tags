
#' with_pb tag to use progress bar with functionals
#'
#' @param format The format of the progress bar. A number of tokens can be used
#'   here, see them below. It defaults to "[:bar] :percent", which means that
#'   the progress bar is within brackets on the left, and the percentage is
#'   printed on the right.
#' @param width Width of the progress bar.
#' @param complete Completion character.
#' @param incomplete Incomplete character.
#' @param current Current character.
#' @param callback Callback function to call when the progress bar finishes.
#'   The progress bar object itself is passed to it as the single parameter.
#' @param clear Whether to clear the progress bar on completion.
#' @param show_after Amount of time in seconds, after which the progress bar is
#'   shown on the screen. For very short processes, it is probably not worth
#'   showing it at all.
#' @param force Whether to force showing the progress bar, even if the given (or default) stream does not seem to support it.
#' @param tokens A list of unevaluated expressions, using `alist`, to be passed
#'   passed to the `tick` method of the progress bar
#' @param message A message to display on top of the bar
#' @param FUN_ARG name or position of function arg, by default looks for the
#'   first argument that is either `FUN`, `.f` or `f`
#' @export
#' @examples
#' with_pb$map(1:5, ~{Sys.sleep(1);.x*2})
#' with_pb(format = "[:bar] :eta")$lapply(1:5, function(x) {Sys.sleep(1);x*2})
with_pb <- tag(args = alist(
  format = "[:bar] :percent",
  width = options("width")[[1]] - 2,
  complete = "=",
  incomplete = "-",
  current =">",
  callback = invisible, # doc doesn't give default but this seems to work ok
  clear = TRUE,
  show_after = .2,
  force = FALSE,
  # The only arg not forwarded to progress::progress_bar$new()
  # By default `for` will self detruct after being called
  message = NULL,
  tokens = alist(),
  FUN_ARG=NULL),{
  #  browser()
  if (is.null(FUN_ARG))
    FUN_ARG <- min(which(names(CALL) %in% c("FUN",".f","f")))
  seq <- eval(ARGS[[1]])

  fun <- rlang::as_function(eval(CALL[[FUN_ARG]]))

  CALL[[FUN_ARG]] <- tracing(exit = quote({
    pb$tick()
    }))$fun

  pb <- progress::progress_bar$new(
    format = format, width = width, complete = complete,
    incomplete = incomplete, current = current,
    callback = callback,
    clear = clear, show_after = show_after, force = force,
    total = length(seq))

  if(!is.null(message)) pb$message(message)
  eval(CALL)
})



