
#' safely2  tag
#'
#' similar to `purrr::safely`
#' @inheritParams purrr::safely
#' @export
#' @examples
#' safely2$log(10, otherwise = NA_real_)
#' safely2$log("a", otherwise = NA_real_)
#' safely2(otherwise = NA_real_)$log(10)
#' safely2(otherwise = NA_real_)$log("a")
safely2 <- tag(args = alist(otherwise = NULL, quiet = TRUE),{
  tags:::capture_error(eval.parent(CALL), otherwise, quiet)
})

#' quietly2  tag
#'
#' similar to `purrr::quietly`
#' @inheritParams purrr::quietly
#' @export
#' @examples
#' quietly2$sqrt(-1)
#' quietly2$sqrt(4)
quietly2 <- as_tag_adverb(purrr::quietly)


#' possibly2  tag
#'
#' similar to `purrr::possibly`
#' @inheritParams purrr::possibly
#' @export
#' @examples
#' possibly2$log("a", otherwise = NA_real_)
#' possibly2$log(10, otherwise = NA_real_)
possibly2 <- tag(args = alist(otherwise=, quiet = TRUE),{
  tryCatch(eval.parent(CALL), error = function(e) {
    if (!quiet)
      message("Error: ", e$message)
    otherwise
  }, interrupt = function(e) {
    stop("Terminated by user", call. = FALSE)
  })
})


