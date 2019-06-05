# 31 functions

with_to_setting <- function(fun_chr){
  # args0: list of default arguments of the function
  args0 <- head(as.list(args(
    eval(bquote(`::`(withr, .(fun_chr))))
  )),-1)
  # args1: mapping of items from args0 to dotted names (dot added if didn't exist)
  args1 <- purrr::imap(args0, ~rlang::sym(sub("^\\.\\.",".", paste0(".",.y))))
  args1[["..."]] <- NULL
  args1$code <- quote(CALL(eval=FALSE))
  args1 <- lapply(args1, function(x) bquote(!!.(x)))
  fmls <- formals(getFromNamespace(fun_chr,"withr"))[-2]
  args1$help <- NULL
  fmls$help <- NULL
  names(fmls) <- sub("^\\.\\.",".", paste0(".", names(fmls)))
  if("..." %in% names(fmls)){
    names(fmls)[names(fmls) == "..."] <- "..dots"
    fmls["..dots"] <- alist(..dots = NULL)
    CALL0 <- as.call(c(bquote(`::`(withr, .(fun_chr))), args1, alist(!!!..dots)))

  } else {
    CALL0 <- as.call(c(bquote(`::`(withr, .(fun_chr))), args1))
  }
  CALL0 <- bquote(eval.parent(rlang::expr(.(CALL0))))
  # to deal with argument `help` which is not useful and doesn't have a default

  eval(rlang::expr(tag(args = fmls,{
    if(!requireNamespace("withr"))
      stop("The package withr must be installed to use `tags::setting_*` functions")
    !!CALL0})))
}

# create all functions programatically
# library(withr)
# all_withr_funs <- ls(as.environment("package:withr"))
# detach("package:withr")
# with_funs <- all_withr_funs[startsWith(all_withr_funs, "with")][-1]
with_funs <- c(
  "with_bmp", "with_cairo_pdf", "with_cairo_ps", "with_collate",
  "with_connection", "with_db_connection", "with_dir", "with_environment",
  "with_envvar", "with_file", "with_jpeg", "with_libpaths", "with_locale",
  "with_makevars", "with_message_sink", "with_namespace", "with_options",
  "with_output_sink", "with_package", "with_par", "with_path",
  "with_pdf", "with_png", "with_postscript", "with_preserve_seed",
  "with_seed", "with_svg", "with_temp_libpaths", "with_tempfile",
  "with_tiff", "with_xfig")
eval(parse(text=sprintf('setting_%s <- with_to_setting("%s")', sub("with_","",with_funs) , with_funs)))



#' tag counterparts to withr functions
#'
#' These functions are wrapped around functions from the *withr* package.
#' While in *withr* function names are prefixed with `"with_"` in this package
#' they are prefixed with `"setting_"`.
#'
#' *withr* makes working with global state in R safer and less error prone.
#' With *withr* the changes are made during the evaluation of an expression,
#' with the `tag` counterparts they are made during a function call.
#'
#' Note that `withr::with_options` and `rlang::with_options` are different
#' functions, and `tags::setting_options` is a wrapper around `withr::with_options`.
#'
#' @name withr_wrappers
#' @param .new,..dots,.con,.env,.pos,.name,.warn.conflicts,.action,.file,.path,.assignment,.append,.package,.split,.lib.loc,.character.only,.logical.return,.quietly,.verbose,.no.readonly,.width,.height,.onefile,.family,.title,.fonts,.version,.paper,.encoding,.bg,.fg,.pointsize,.pagecentre,.colormodel,.useDingbats,.useKerning,.fillOddEven,.compress,.horizontal,.print.it,.command,.code,.seed,.antialias,.envir,.pattern,.tmpdir,.fileext,.defaultfont,.textspecial forwarded to the relevant *withr* function argument
#' @examples
#' # examples adapted from withr's vignette
#'
#' \dontrun{
#' ## Graphics devices
#' path <- "test.png"
#' with_png(path, width = 400, height = 300, {
#'   plot(mtcars$hp, mtcars$mpg)
#' })
#' setting_png(path, list(width = 400, height = 300))$plot(mtcars$hp, mtcars$mpg)
#'
#' ## Connections
#'
#' with_connection(list(con = file("temp", "w")), {
#'   writeLines(c("foo", "bar"), con)
#' })
#' unlink("temp")
#'
#' setting_connection(list(con = file("temp", "w")))$writeLines(c("foo", "bar"), con)
#' unlink("temp")
#'
#' ## Packages
#'
#' with_package("lattice", {
#' xyplot(y ~ x, data.frame(x = -2:2, y = dnorm(-2:2)))
#' })
#'
#' # setting_package cannot be used directly on package function, to do this use `::`
#' fun <- function(x, y){ xyplot(y ~ x, data.frame(x = x, y = y))}
#' setting_package("lattice")$fun(x = -2:2, y = dnorm(-2:2))
#'
#' ## Tempfiles
#'
#' with_tempfile("file1", {
#'   print(file1)
#'   writeLines("foo", file1)
#'   readLines(file1)
#' })
#'
#' fun <- function(file0) {
#'   print(file0)
#'   writeLines("foo", file0)
#'   readLines(file0)
#' }
#' setting_tempfile("file1")$fun(file1)
#' setting_tempfile("temp")$fun(temp)
#' }
NULL

# cat(sprintf("#' @rdname withr_wrappers\n#' @export\n%s <- %s",
#             sub("with","setting",with_funs),
#             sub("with","setting",with_funs)),
#     sep="\n\n")


# setting_environment has an argument which depends on another argument
# so it needs to be fixed
def <- attr(setting_environment,"definition")
def[["args"]]$.name <- quote(format(.env))
setting_environment <- eval(def)

#' @rdname withr_wrappers
#' @export
setting_bmp <- setting_bmp

#' @rdname withr_wrappers
#' @export
setting_cairo_pdf <- setting_cairo_pdf

#' @rdname withr_wrappers
#' @export
setting_cairo_ps <- setting_cairo_ps

#' @rdname withr_wrappers
#' @export
setting_collate <- setting_collate

#' @rdname withr_wrappers
#' @export
setting_connection <- setting_connection

#' @rdname withr_wrappers
#' @export
setting_db_connection <- setting_db_connection

#' @rdname withr_wrappers
#' @export
setting_dir <- setting_dir

#' @rdname withr_wrappers
#' @export
setting_environment <- setting_environment

#' @rdname withr_wrappers
#' @export
setting_envvar <- setting_envvar

#' @rdname withr_wrappers
#' @export
setting_file <- setting_file

#' @rdname withr_wrappers
#' @export
setting_jpeg <- setting_jpeg

#' @rdname withr_wrappers
#' @export
setting_libpaths <- setting_libpaths

#' @rdname withr_wrappers
#' @export
setting_locale <- setting_locale

#' @rdname withr_wrappers
#' @export
setting_makevars <- setting_makevars

#' @rdname withr_wrappers
#' @export
setting_message_sink <- setting_message_sink

#' @rdname withr_wrappers
#' @export
setting_namespace <- setting_namespace

#' @rdname withr_wrappers
#' @export
setting_options <- setting_options

#' @rdname withr_wrappers
#' @export
setting_output_sink <- setting_output_sink

#' @rdname withr_wrappers
#' @export
setting_package <- setting_package

#' @rdname withr_wrappers
#' @export
setting_par <- setting_par

#' @rdname withr_wrappers
#' @export
setting_path <- setting_path

#' @rdname withr_wrappers
#' @export
setting_pdf <- setting_pdf

#' @rdname withr_wrappers
#' @export
setting_png <- setting_png

#' @rdname withr_wrappers
#' @export
setting_postscript <- setting_postscript

#' @rdname withr_wrappers
#' @export
setting_preserve_seed <- setting_preserve_seed

#' @rdname withr_wrappers
#' @export
setting_seed <- setting_seed

#' @rdname withr_wrappers
#' @export
setting_svg <- setting_svg

#' @rdname withr_wrappers
#' @export
setting_temp_libpaths <- setting_temp_libpaths

#' @rdname withr_wrappers
#' @export
setting_tempfile <- setting_tempfile

#' @rdname withr_wrappers
#' @export
setting_tiff <- setting_tiff

#' @rdname withr_wrappers
#' @export
setting_xfig <- setting_xfig


# cleanup!
rm(with_to_setting, with_funs)
