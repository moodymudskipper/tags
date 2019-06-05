# similar to dot operations but for formals
# selecting_formals can reorder the function, rename on the go, and possibly partialize (we can use partial for these cases)
# mutating_formals changes the default values, we can create new ones in principle but not sure how it would go
# transmuting
# this should take the call, add the implicit formals, and apply the selection


# selecting_formals0 <- function(f, ...vars){
#   # formals of the input function
#   fmls1 <- tags:::formals2(f)
#   names(fmls1)[names(fmls1) == "..."] <- "...dots"
#   # some of them might be empty and it'll cause issues, we replace them
#   # with quoted rlang::missing_arg()
#   fmls1 <- lapply(fmls1, function(x) if (identical(x, rlang::missing_arg()))
#     quote( ...empty) else x)
#   # fmls1[sapply(fmls1, identical, rlang::missing_arg())] <-
#   #   quote(rlang::missing_arg())
#   # to use dplyr functions we need to convert to tibble, but a tibble doesn't
#   # accept call columns so we nest them in lists and take them back out
#   fmls2 <- purrr::map(select(
#     tibble::as_tibble(purrr::map(fmls1,list), .name_repair = "minimal"),
#     !!!...vars),1)
#
#   names(fmls2)[names(fmls2) == "...dots"] <- "..."
#   fmls2 <- lapply(fmls2, function(x) if (identical(x, quote( ...empty))) rlang::missing_arg() else x)
#   # if("..." %in% names(fmls1) && !"..." %in% names(fmls2))
#   #   fmls2 <- c(fmls2, alist(...=))
#   partial_args <- setdiff(names(fmls1), names(fmls2))
#   f <- purrr::partial(f, !!fmls1[partial_args])
#   formals(f) <- fmls2
#   f
# }


# selecting_formals(head, vars(y = x))(cars,3)
#
# selecting_formals <- function(f, ...vars){
#   browser()
#   # formals of the input function
#   fmls1 <- tags:::formals2(f)
#   # some of them might be empty and it'll cause issues, we replace them
#   # with quoted rlang::missing_arg()
#   fmls1 <- lapply(fmls1, function(x) if (identical(x, rlang::missing_arg()))
#     quote( rlang::missing_arg()) else x)
#   # fmls1[sapply(fmls1, identical, rlang::missing_arg())] <-
#   #   quote(rlang::missing_arg())
#   # to use dplyr functions we need to convert to tibble, but a tibble doesn't
#   # accept call columns so we nest them in lists and take them back out
#   fmls2 <- purrr::map(select(
#     tibble::as_tibble(purrr::map(fmls1,list), .name_repair = "minimal"),
#     !!!...vars),1)
#
#   f2 <- as.function(c(fmls2,quote({
# # we need to turn around ...vars/fmls2 so y=x becomes x = y, then feed arguments to
# # the original f
#
#
#   })))
#
#
#
# }
