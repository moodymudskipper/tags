
<!-- README.md is generated from README.Rmd. Please edit that file -->
tags
====

This package consists of a collection of tags built using the functions and principle designed and described in the package *tag*. See the read me at : <https://github.com/moodymudskipper/tag>.

It is still largely in progress and subject to important changes such as addition /removal/ renaming of functions and arguments etc.

We showcase hereby the tags from the package.

Installation and setup :

``` r
# devtools::install_github("moodymudskipper/tag")
# devtools::install_github("moodymudskipper/tags")
library(tags)
library(tidyverse,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
#> Registered S3 methods overwritten by 'ggplot2':
#>   method         from 
#>   [.quosures     rlang
#>   c.quosures     rlang
#>   print.quosures rlang
#> Registered S3 method overwritten by 'rvest':
#>   method            from
#>   read_xml.response xml2
#> -- Attaching packages --------------------------------------------------------------------------------------------- tidyverse 1.2.1 --
#> v ggplot2 3.1.1       v purrr   0.3.2  
#> v tibble  2.1.1       v dplyr   0.8.0.1
#> v tidyr   0.8.3       v stringr 1.4.0  
#> v readr   1.3.1       v forcats 0.4.0
#> -- Conflicts ------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(progress)
```

### Basic tags

#### `strictly` tag to adjust strictness of a call

``` r
strictly(-1)$sqrt(-1)
#> [1] NaN
```

#### `dbg` tag to debug a call

Will run `debugonce` on the input function.

``` r
dbg$sample(5)
```

#### `logging` tag to log call and time it took

``` r
logging$head(cars,2)
#> logging$head(x = cars, 2)
#>   ~ ... 0 sec
#>   speed dist
#> 1     4    2
#> 2     4   10
```

#### `trying` tag to try a call or if failure call alternate call

``` r
trying$paste("hello","world", .else = "hi")
#> [1] "hello world"
trying$paste("hello", world, .else = "hi")
#> Error in (function (..., sep = " ", collapse = NULL)  : 
#>   object 'world' not found
#> [1] "hi"
trying$paste("hello", world, .else = "hi", .silent = TRUE)
#> [1] "hi"
```

#### `preserving_attr` tag to preserve attributes

``` r
preserving_attr$map_dfr(head(iris,2),identity)
#> # A tibble: 2 x 5
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> *        <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#> 1          5.1         3.5          1.4         0.2 setosa 
#> 2          4.9         3            1.4         0.2 setosa
preserving_attr(incl_class = TRUE)$map_dfr(head(iris,2),identity)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
```

### Tidy tags

#### `bng` tag to enable tidy evaluation or arguments

``` r
u <- "speed"
v <- quote(dist)
w <- rlang::quo(time)
x <- list(a=c(1, 2), b=c(3, 4))
bng$transform(head(cars,2), !!w := !!v / !!rlang::sym(u), !!!x)
#>   speed dist time a b
#> 1     4    2  0.5 1 3
#> 2     4   10  2.5 2 4
```

#### `lbd` tag to use formula notation on FUN arguments

``` r
lbd$ave(c(1,2,4,3,NA),c(1,1,2,2,2),FUN = ~mean(.,na.rm=TRUE))
#> [1] 1.5 1.5 3.5 3.5 3.5
```

#### `on_self` tag to allow self referencing in arguments

``` r
on_self$transform(head(iris,2), Petal.Width = ~1000*(.), Species = ~toupper(.))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         200  SETOSA
#> 2          4.9         3.0          1.4         200  SETOSA
on_self$summarize(iris, Petal.Width = ~median(.), Sepal.Length = ~mean(.))
#>   Petal.Width Sepal.Length
#> 1         1.3     5.843333
```

#### `grp` tag to group and ungroup around another operation

``` r
tbl_at_syms <- tags:::tbl_at_syms
grp$summarize(iris,meanSL = mean(Sepal.Length), .by="Species")
#> # A tibble: 3 x 2
#>   Species    meanSL
#>   <fct>       <dbl>
#> 1 setosa       5.01
#> 2 versicolor   5.94
#> 3 virginica    6.59
grp(.by="Species")$summarize(iris,meanSL = mean(Sepal.Length))
#> # A tibble: 3 x 2
#>   Species    meanSL
#>   <fct>       <dbl>
#> 1 setosa       5.01
#> 2 versicolor   5.94
#> 3 virginica    6.59
grp$summarize_all(iris, mean, .by="Species")
#> # A tibble: 3 x 5
#>   Species    Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   <fct>             <dbl>       <dbl>        <dbl>       <dbl>
#> 1 setosa             5.01        3.43         1.46       0.246
#> 2 versicolor         5.94        2.77         4.26       1.33 
#> 3 virginica          6.59        2.97         5.55       2.03
grp$slice(iris,1, .by="Species")
#> # A tibble: 3 x 5
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species   
#>          <dbl>       <dbl>        <dbl>       <dbl> <fct>     
#> 1          5.1         3.5          1.4         0.2 setosa    
#> 2          7           3.2          4.7         1.4 versicolor
#> 3          6.3         3.3          6           2.5 virginica
# unnamed groupings are not kept
grp$summarize(
  iris,meanSW = mean(Sepal.Width), .by= vars(Species, Sepal.Width > 3.2))
#> # A tibble: 6 x 2
#>   Species    meanSW
#>   <fct>       <dbl>
#> 1 setosa       3.04
#> 2 setosa       3.63
#> 3 versicolor   2.75
#> 4 versicolor   3.35
#> 5 virginica    2.88
#> 6 virginica    3.49
# named groupings are kept
grp$summarize(
  iris,meanSW = mean(Sepal.Width), .by= vars(Species, long_sepal = Sepal.Width > 3.2))
#> # A tibble: 6 x 3
#>   Species    long_sepal meanSW
#>   <fct>      <lgl>       <dbl>
#> 1 setosa     FALSE        3.04
#> 2 setosa     TRUE         3.63
#> 3 versicolor FALSE        2.75
#> 4 versicolor TRUE         3.35
#> 5 virginica  FALSE        2.88
#> 6 virginica  TRUE         3.49
```

#### `rw` tag to apply a transformation rowwise

``` r
rw$mutate(head(iris,3),X = mean(c(Sepal.Length, Sepal.Width)))
#> # A tibble: 3 x 6
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species     X
#>          <dbl>       <dbl>        <dbl>       <dbl> <fct>   <dbl>
#> 1          5.1         3.5          1.4         0.2 setosa   4.3 
#> 2          4.9         3            1.4         0.2 setosa   3.95
#> 3          4.7         3.2          1.3         0.2 setosa   3.95
mutate(head(iris,3),X = mean(c(Sepal.Length, Sepal.Width)))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species        X
#> 1          5.1         3.5          1.4         0.2  setosa 4.066667
#> 2          4.9         3.0          1.4         0.2  setosa 4.066667
#> 3          4.7         3.2          1.3         0.2  setosa 4.066667
```

*purrr* adverbs counterparts
----------------------------

``` r
safely2$log(10, otherwise = NA_real_)
#> $result
#> [1] 2.302585
#> 
#> $error
#> NULL
safely2$log("a", otherwise = NA_real_)
#> $result
#> [1] NA
#> 
#> $error
#> <simpleError in .Primitive("log")(x = "a"): non-numeric argument to mathematical function>
safely2(otherwise = NA_real_)$log(10)
#> $result
#> [1] 2.302585
#> 
#> $error
#> NULL
safely2(otherwise = NA_real_)$log("a")
#> $result
#> [1] NA
#> 
#> $error
#> <simpleError in .Primitive("log")(x = "a"): non-numeric argument to mathematical function>
```

``` r
quietly2$sqrt(-1)
#> $result
#> [1] NaN
#> 
#> $output
#> [1] ""
#> 
#> $warnings
#> [1] "NaNs produced"
#> 
#> $messages
#> character(0)
quietly2$sqrt(4)
#> $result
#> [1] 2
#> 
#> $output
#> [1] ""
#> 
#> $warnings
#> character(0)
#> 
#> $messages
#> character(0)
```

``` r
possibly2$log("a", otherwise = NA_real_)
#> [1] NA
possibly2$log(10, otherwise = NA_real_)
#> [1] 2.302585
```

functional sequences
--------------------

This tag is not yet coded consistently to the other tags.

``` r
fs(head)$dim()(iris)
#> [1] 6 5
fs$head(2)$gsub("h","X",.)(c("hello","hi","foo"))
#> [1] "Xello" "Xi"
```

dot operations
--------------

`selecting_dots`, `renaming_dots`, `mutating_dots`, and `transmuting_dots` are wrappers around *dplyr* functions.

`reversing_dots` flips the arguments fed to the `...`.

``` r
# the functions can be used in regular cal
reversing_dots$paste0("a", "b")
#> [1] "ba"
selecting_dots(vars(b2,starts_with("a")))$
  transform(head(cars,2), a1 = 1, b1 = 2, a2 = 3, b2 = speed *100)
#>   speed dist  b2 a1 a2
#> 1     4    2 400  1  3
#> 2     4   10 400  1  3
mutating_dots(exprs(foo = 1000 * bar))$
  mutate(head(cars,2), bar = speed * 10)
#>   speed dist bar   foo
#> 1     4    2  40 40000
#> 2     4   10  40 40000
# or with forwarded dots
fun <- function(...){
  print(
    selecting_dots(vars(b2,starts_with("a")))$
      mutate(head(cars,2), ...))
  print(
    renaming_dots(vars(foo = a1))$
      mutate(head(cars,2), ...))
  print(
    mutating_dots(exprs(a2 = 100*a2 + a1, foo = 1000 * b1))$
      mutate(head(cars,2), ...))
  print(
    transmuting_dots(exprs(a2 = 100*a2 + a1, foo = 1000 * b1))$
      mutate(head(cars,2), ...))
  print(
    reversing_dots$mutate(head(cars,2), ...))
}
fun(a1 = 1, b1 = 2, a2 = 3, b2 = 40)
#>   speed dist b2 a1 a2
#> 1     4    2 40  1  3
#> 2     4   10 40  1  3
#>   speed dist foo b1 a2 b2
#> 1     4    2   1  2  3 40
#> 2     4   10   1  2  3 40
#>   speed dist a1 b1  a2 b2  foo
#> 1     4    2  1  2 301 40 2000
#> 2     4   10  1  2 301 40 2000
#>   speed dist  a2  foo
#> 1     4    2 301 2000
#> 2     4   10 301 2000
#>   speed dist b2 a2 b1 a1
#> 1     4    2 40  3  2  1
#> 2     4   10 40  3  2  1
```

formal operations
-----------------

**Work in progress**

`selecting_args`, `renaming_args`, `mutating_args`, and `transmuting_args` will change the formals of the functions.

They cannot be coded using the `tag` function only.

using a progress bar
--------------------

Detects the functional and displays a progress bar, using all the available options from package *progress*.

``` r
with_pb$map(1:5, ~{Sys.sleep(1);.x*2})
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 4
#> 
#> [[3]]
#> [1] 6
#> 
#> [[4]]
#> [1] 8
#> 
#> [[5]]
#> [1] 10
with_pb$lapply(1:5, function(x) {Sys.sleep(1);x*2})
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 4
#> 
#> [[3]]
#> [1] 6
#> 
#> [[4]]
#> [1] 8
#> 
#> [[5]]
#> [1] 10
```

tracing and enclosing
---------------------

`tracing` is modelled after `base::trace` and edits the body of its input function while `enclosing` wraps the input function call inside a call to function `.around`, between statements `.before` and `.after`.

``` r
tracing(quote(print("hello")), exit = quote(print("good bye")))$mean(1:5)
#> [1] "hello"
#> [1] "good bye"
#> [1] 3
enclosing(.before = print("hello"),
          .after = print(paste("result was:", .)),
          .around = suppressWarnings)$sqrt(-1)
#> [1] "hello"
#> [1] "result was: NaN"
#> [1] NaN
```

`in_parallel` tag to execute a call in another session
------------------------------------------------------

When using this tag the function and its arguments are evaluated and saved so the call can be executed in another session, it allows things like displaying time elapsed or % of esimated time, or setting a timeout.

**work in progress** (but already works with limited features)

``` r
u <- cars
v <- 3
fun <- function(y,m){
  Sys.sleep(5)
  head(y, m)
}
x <- in_parralel(.wait=FALSE)$fun(u,v)
#> C:/PROGRA~1/R/R-36~1.0/bin/RScript.exe "C:/R/00 packages/tags/in_a_new_session_script.R"
x
#> NULL
y <- in_parralel$fun(u,v)
#> C:/PROGRA~1/R/R-36~1.0/bin/RScript.exe "C:/R/00 packages/tags/in_a_new_session_script.R"
#>   ~ 1.02 secs             ~ 2.04 secs             ~ 3.04 secs             ~ 4.05 secs             ~ 5.07 secs             ~ 6.08 secs
y
#>   speed dist
#> 1     4    2
#> 2     4   10
#> 3     7    4
```

`using` tag to evaluate call in a given environment
---------------------------------------------------

``` r
using(iris)$ave(Sepal.Length,Species, FUN = mean)
#>   [1] 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006
#>  [12] 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006
#>  [23] 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006
#>  [34] 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006 5.006
#>  [45] 5.006 5.006 5.006 5.006 5.006 5.006 5.936 5.936 5.936 5.936 5.936
#>  [56] 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936
#>  [67] 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936
#>  [78] 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936
#>  [89] 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936 5.936
#> [100] 5.936 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588
#> [111] 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588
#> [122] 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588
#> [133] 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588 6.588
#> [144] 6.588 6.588 6.588 6.588 6.588 6.588 6.588
vec <- c(1:2,a=2.5,3:4)
using(vec)[.>=a] # rather than ; vec[vec >= vec["a"]]
#>   a         
#> 2.5 3.0 4.0
using(vec)[[.>=a]] # rather than ; vec >= vec["a"]
#>                 a             
#> FALSE FALSE  TRUE  TRUE  TRUE
```
