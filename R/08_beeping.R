# 2 functions

#' beeping tag to beep when instruction finishes
#'
#' uses `beepr::beep` to signal that the call was executed.
#'
#' @param .sound character string or number specifying what sound to be played,
#'    forwarded to the `sound` argument of `beepr::beep`
#' @param .expr an optional expression to be excecuted before the sound,
#'   forwarded to the `expr` argument of `beepr::beep`
#' @export
#' @examples
#' \dontrun{
#' beeping$paste("hello","world")
#' beeping(2)$paste("hello","world")
#' beeping("facebook")$paste("hello","world")
#' }
beeping <- tag(args = alist(.sound=1, .expr = NULL), {
  if(!requireNamespace("beepr")) stop("Install the beepr package to use the tag 'beeping'")
  on.exit(beepr::beep(.sound, .expr))
  CALL()
})

#' popping_up tag to trigger message box when instruction finishes
#'
#' wraps `tcltk::tk_messageBox` to signal that the call was executed.
#'
#' @param .message character. The information field of the dialog box.
#'    Forwarded to the `message` argument of `tcltk::tk_messageBox`
#' @param .caption the caption on the widget displayed.
#'    Forwarded to the `caption` argument of `tcltk::tk_messageBox`
#' @export
#' @examples
#' \dontrun{
#' popping_up$paste("hello","world")
#' popping_up("a message", "a caption")$paste("hello","world")
#' }
popping_up <- tag(args = alist(.message = NULL, .caption =""), {
  if(!requireNamespace("tcltk")) stop("Install the tcltk package to use the tag 'popping_up'")
  on.exit({
    if(is.null(.message)) .message = paste("Done:", rlang::expr_text(sys.call()))
    tcltk::tk_messageBox("ok",.message, .caption)
  })
  CALL()
})
