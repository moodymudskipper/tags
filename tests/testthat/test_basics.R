expect_no_error <- purrr::partial(expect_error, regexp = NA)

test_that("logging tag works",{
  capture.output({
  expect_equal(logging$identity("foo"),"foo")
  expect_message(logging$identity("foo"), 'logging$identity("foo")', fixed= TRUE)
  fun <- function(t, f, y) t$f(y)
  expect_error(fun(logging, identity,"foo"), NA)
  })
})

# test_that("strictly tag works",{
#   expect_error(strictly$sqrt(-1))
#   expect_error(strictly(2)$sqrt(-1))
#   expect_warning(strictly(1)$sqrt(-1))
#   expect_warning(strictly(0)$sqrt(-1))
#   suppressWarnings({
#     expect_no_error(strictly(-1)$sqrt(-1))
#     # dealing with environments
#     fun1 <- function(t1, t_arg, f, y) t1(t_arg)$f(y)
#     expect_no_error(fun1(strictly, -1, sqrt,-1))
#     fun2 <- function(t1, t_arg, f, y) t1()(f, t_arg)(y)
#     expect_no_error(fun2(strictly, -1, sqrt,-1))
#     fun3 <- function(t1, t_arg, f, y) t1$f(y, t_arg)
#     expect_no_error(fun3(strictly, -1, sqrt,-1))
#   })
# })

# test_that("dbg tag works",{
#   # standard evaluation
#   dbg$mean(1:5)
#   # non standard evaluation
#   dbg$subset(iris, Sepal.Width >= 3.8)
#   # dealing with environments
#   fun1 <- function(t1, f, y) t1$f(y,Sepal.Width >= 3.8)
#   expect_no_error(fun1(dbg, subset, iris))
#   fun2 <- function(t1, f, y) t1()$f(y,Sepal.Width >= 3.8)
#   expect_no_error(fun2(dbg, subset, iris))
#   fun3 <- function(t1, f, y) t1()(f)(y,Sepal.Width >= 3.8)
#   expect_no_error(fun3(dbg, subset, iris))
# })

# test_that("viewing tag works",{
#   expect_no_error(viewing$subset(iris,Sepal.Width >= 3.8))
#   expect_no_error(viewing("foo")$subset(iris,Sepal.Width >= 3.8))
#   expect_no_error(viewing()(subset,"foo")(iris,Sepal.Width >= 3.8))
#   expect_no_error(viewing$subset(iris,Sepal.Width >= 3.8, .title = "foo"))
#   # dealing with environments
#   fun1 <- function(t1, f, y) t1$f(y,Sepal.Width >= 3.8)
#   expect_no_error(fun1(viewing, subset, iris))
#   fun2 <- function(t1, f, y) t1()$f(y,Sepal.Width >= 3.8)
#   expect_no_error(fun2(viewing, subset, iris))
#   fun3 <- function(t1, f, y) t1()(f)(y,Sepal.Width >= 3.8)
#   expect_no_error(fun3(viewing, subset, iris))
#   fun4 <- function(t1, t_arg, f, y) t1$f(y,Sepal.Width >= 3.8, .title = t_arg)
#   expect_no_error(fun4(viewing, "foo", subset, iris))
#   fun5 <- function(t1, t_arg, f, y) t1(t_arg)$f(y,Sepal.Width >= 3.8)
#   expect_no_error(fun5(viewing, "foo", subset, iris))
#   fun6 <- function(t1, t_arg, f, y) t1()(f, t_arg)(y,Sepal.Width >= 3.8)
#   expect_no_error(fun6(viewing, "foo", subset, iris))
# })


# test_that("trying tag works",{
#   expect_equal(trying$paste("hello","world", .else = "hi"),
#                 "hello world")
#   expect_equal(trying$paste("hello", world, .else = "hi", .silent = TRUE),
#                 "hi")
#   # dealing with environments
#   fun1 <- function(t1, e, s, f, ...) t1(e,s)$f(...)
#   expect_equal(fun1(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun2 <- function(t1, e, s, f, ...) t1(e)(f,s)(...)
#   expect_equal(fun2(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun3 <- function(t1, e, s, f, ...) t1(.silent = s)(f,e)(...)
#   expect_equal(fun3(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun4 <- function(t1, e, s, f, ...) t1()(f,e,s)(...)
#   expect_equal(fun4(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun5 <- function(t1, e, s, f, ...) t1$f(..., .else = e, .silent = s)
#   expect_equal(fun5(trying,  "hi", TRUE , paste, "hello",world), "hi")
#   fun6 <- function(t1, e, s, f, ...) t1(.silent=s)$f(..., .else = e)
#   expect_equal(fun6(trying,  "hi", TRUE , paste, "hello",world), "hi")
# })


test_that("preserving_attr tag works",{
  expect_equal(
    class(preserving_attr(.incl_class = TRUE)(purrr::map_dfr)(head(iris,2),identity)),
    "data.frame")
})
