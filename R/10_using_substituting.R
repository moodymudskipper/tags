
#' using tag to evaluate call in a given environment
#'
#' This is the tag counterpart of `with` and it comes with a handy method for
#' `[`.
#'
#' @param .envir environment, list or vector on which to restrict the call
#' @export
#' @examples
#' using(iris)$ave(Sepal.Length,Species, FUN = mean)
#' vec <- c(1:2,a=2.5,3:4)
#' using(vec)[.>=a] # rather than ; vec[vec >= vec["a"]]
#' using(vec)[[.>=a]] # rather than ; vec >= vec["a"]
using <- tag(args= alist(.envir=),{
  if(is.atomic(.envir)) .envir <- as.list(.envir)
  eval(CALL, envir = .envir)
})
# we can't give the tag adverb a class and attribute through the use of the `tag`
# function alone so we edit the body afterwards
body(using)[[5]] <- quote(attr(tag_adv, ".envir") <- .envir)
body(using)[[6]] <- quote(add_class(tag_adv, "tag_adverb", "tag_using"))
# and define a method for `[`

#' @export
`[[.tag_using` <- function(x,i){
  call <- substitute(x(identity)(i))
  call <- do.call(substitute, list(call, list(. = attr(x, ".envir"))))
  eval.parent(call)
}

#' @export
`[.tag_using` <- function(x,i){
  call <- substitute(x(identity)(i))
  call <- do.call(substitute, list(call, list(. = attr(x, ".envir"))))
  attr(x, ".envir")[eval.parent(call)]
}
