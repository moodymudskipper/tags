#' parse arguments of function before execution
#'
#' A wrapper around `readr::parse_guess` to help deal easily with unconverted
#' user input.
#'
#' Character inputs that are not parsed to other types by `readr::parse_guess`
#' are evaluated with `eval(parse(text=...))` if `.eval` is set to `TRUE`.
#'
#' Non character inputs will not be converted, however they will be evaluated if
#' they are mentionned in `.args` or if `.args` is `NULL`
#'
#' @param .args character vector of arguments to consider, if NULL (default) all
#'   arguments are candidates to be parsed.
#' @param na Character vector of strings to interpret as missing values. Set this option to character() to indicate no missing values.
#' @param locale	The locale controls defaults that vary from place to place. The default locale is US-centric (like R), but you can use locale() to create your own locale that controls things like the default time zone, encoding, decimal mark, big mark, and day/month names.
#' @param trim_ws	Should leading and trailing whitespace be trimmed from each field before parsing it?
#' @param guess_integer	If TRUE, guess integer types for whole numbers, if FALSE guess numeric type for all numbers.
#' @export
#' @examples
#' parsing_args(.eval=TRUE)$head("cars", n= "2")
#' parsing_args(.args = "n")$head(cars, n= "2")
#' parsing_args$head(cars, n= "2")
parsing_args <- tag::tag(args = alist(
  .args = NULL,
  .eval = FALSE,
  .na = c("", "NA"),
  .locale = readr::default_locale(),
  .trim_ws = TRUE,
  .guess_integer = FALSE),{
    args <- F_ARGS(eval=FALSE,type= "unexpanded")
    has_dots <- "..." %in% names(args)
    if(has_dots) {
      dot_args <- args[["..."]]
      if(!is.null(.args)) dot_args <- dot_args[.args]
      dot_args <- eval.parent(dot_args)
      which_chr <- sapply(dot_args, is.character)
      dot_args <- dot_args[which_chr]
      dot_args <- lapply(dot_args, readr::parse_guess, .na, .locale, .trim_ws, .guess_integer)
      if (.eval){
        dot_args <- lapply(dot_args, function(arg) {
          if(is.character(arg))
            eval(parse(text = arg, parent.frame(2)))
          else
            arg
        })
      }
    }
    if(!is.null(.args)) args <- args[.args]
    args <- eval.parent(args)
    args <- args[sapply(args, is.character)]
    args <- lapply(args, readr::parse_guess, .na, .locale, .trim_ws, .guess_integer)
    if (.eval){
      args <- lapply(args, function(arg) {
        if(is.character(arg))
          eval(parse(text = arg, parent.frame(2)))
        else
          arg
      })
    }
    call <- CALL(eval = FALSE, type= "unexpanded")
    call[names(args)] <- args
    if(has_dots) {
      call_dots <- call[["..."]]
      call_dots[which_chr] <- dot_args
      call <- c(as.list(call), call_dots)
      call[["..."]] <- NULL
      call <- as.call(call)
    }
    eval.parent(call)
  })



