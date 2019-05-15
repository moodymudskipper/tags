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

# fs can be a tag with zero arg rather than a tag_adverb, it will create another
# tag with the args from head, wch


#' fs tag_adverb to start a dollar piped functional sequence
#'
#' Based on `%>%` from the package magrittr
#'
#' @param x a function
#'
#' @return a tagged function
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#'\dontrun{
#' fs(head)$dim()(iris)
#' fs$head(2)$gsub("h","X",.)(c("hello","hi","foo"))##
#' }
fs <- function(x) {
  res <- eval.parent(substitute((. %>% identity)$x))
  add_class(res, "tag_functional_sequence")
}
fs <- add_class(fs, "tag_adverb")

# x <- quote(fs$head(2))
# dollar_to_pipe(x)
# dollar_to_pipe <- function(x){
#   call <- do.call(substitute, list(x, list(`$` = quote(`%>%`))))
#   call <- as.list(call)
#   call[[1]] <- as.list(call[[1]])
#   call[[c(1,3)]] <- c(call[[c(1, 3)]], call[-1])
#   call <- call[[1]]
#   call[[3]] <- as.call(call[[3]])
#   call <- as.call(call)
#   call
# }

#' @method $ fseq
#' @export
#' @importFrom magrittr "%>%"
#' @rdname dollar_methods
`$.fseq` <- function(e1,e2) {
  # define `$` as a pipe operator as used in functional sequences
  # the first eval substitute will substitute e2, but not e1
  res <- eval(substitute(
    as.function(alist(...=,{
      # the second eval subtitute is double and will first replace list_call
      # by `list(...)` and then replace `list` by `. %>% e1 %>% e2`
      # then the expression will be deparsed to remove the annoying parenthesis
      # as the expression became `(. %>% e1 %>% e2)(...)`
      # then parsed back and evaluated
      call0 <- eval(substitute(substitute(
        list_call,
        list(list = quote(. %>% e1 %>% e2))),
        list(list_call = substitute(list(...)))))
      call0_str <- deparse(call0)
      fun <- eval(parse(text=sub("\\(","",gsub("\\)\\(","(",call0_str))))
      fun
    })),
    list(e2=substitute(e2))))
  add_class(res,"tag_functional_sequence")
}

#' @export
`$.tag_functional_sequence` <- function(e1,e2){
  eval(substitute(e1()$e2))
}
