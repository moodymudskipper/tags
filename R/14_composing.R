# 1 function

# can we make fs a legit tag / tag_adverb ?
# maybe, the twist is that the tag adverb must build a tag itself and that we
# need a way in the end to stop the chain, which can be done with `[`
# the bad thing is that it's a bit weird, the good thing is that we'd stay consistent
# and would not need to end with `()(args)` when we have no args to partialize
# the last function.
# the bad thing is also that it's a headache to code.
# fs(head)()(gsub)("h","X",.)(c("hello","hi","foo"))
# fs is a tag_adverb here, it has only one f arg
# fs(head) is a tag, it has the parameters from head and will build a tag adverb
# fs(head)(2) is a tag_adverb
# fs(head)(2)(gsub) is a tag
# fs(head)(2)(gsub)("h","X",.) is a tag adverb
# so how do we finally apply it on c("hello","hi","foo") ?
# we could use `[`
# fs(head)()(gsub)("h","X",.)[c("hello","hi","foo")]
# fs$head$("h","X",.)[c("hello","hi","foo")]
# other example :
# fs$head$dim[iris]

#' composing tag to start a dollar piped functional sequence
#'
#' `composing` is a tag wrapped around *magrittr*'s functional sequence
#' feature. It works in similar ways except that it uses `$` rather than
#' `%>%` and doesn't start with a dot.
#'
#' @param x a function
#'
#' @return a tagged function
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#'\dontrun{
#' composing()(head)$dim()(iris)
#' composing$head(2)$gsub("h","X",.)(c("hello","hi","foo"))
#' }
#' @name composing
adv <- function(x) {
  if(!requireNamespace("magrittr"))
    stop("Install the magrittr package to use the tag `composing()`")
  res <- eval.parent(substitute((. %>% identity)$x))
  add_class(res, "tag_functional_sequence")
}
adv <- add_class(adv, "tag_adverb")
composing <- tag::as_tag(adv)
attr(composing, "definition") <-
 bquote({
    adv <- .(adv)
    adv <- add_class(adv, "tag_adverb")
    as_tag(adv)
  })
rm(adv)

#' fseq method for the dollar operator
#' @method $ fseq
#' @param e1 lhs
#' @param e2 rhs
#' @export
#' @importFrom magrittr "%>%"
#' @rdname dollar_methods
`$.fseq` <- function(e1,e2) {
  # define `$` as a pipe operator as used in functional sequences
  # the first eval substitute will substitute e2, but not e1
  res <- eval(substitute(
    as.function(alist(...=,{
      fun <- quote(. %>% e1 %>% e2)
      fun[[3]] <-as.call(c(
        fun[[3]],
        eval(substitute(alist(...)))))
      fun <- eval(fun)
      fun
    })),
    list(e2=as.symbol(substitute(e2)))))
  add_class(res,"tag_functional_sequence")
}

#' @export
`$.tag_functional_sequence` <- function(e1,e2){
  eval(substitute(e1()$e2))
}
