# rox <- function(f){
#   cat("#'", substitute(f), "\n#'\n")
#   cat(paste("#' @param", formalArgs(f), collapse="\n"))
#   cat("\n#' @export\n#' @examples\n")
# }

# imports
# @import tags tag
NULL

# tag <- getFromNamespace("tag","tags")

tag <- getFromNamespace("tag","tag")
add_class <- getFromNamespace("add_class","tag")
tbl_at_syms <- getFromNamespace("tbl_at_syms", "dplyr")
capture_error <- getFromNamespace("capture_error","purrr")
capture_output <- getFromNamespace("capture_output","purrr")


