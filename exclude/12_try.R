

#
# # All the following give an identical result, though the use of `$.*` is
# # different and different intermediate objects are built are different
# # Understand all of them and you'll have a good intuitive understanding of
# # what are tags and tag_adverbs
#
# # we could start by describing the canonical form then IDIMATIC 1 AND 2 then
# # "ALL IN ADVERB" and leave others as an exercise, user can think about what
# # methods are used and what intermediate objects are created
#
# # `$.tag` changes trying$paste into trying()$paste (see example coming right after)
# trying$paste("hello",world, .else = toupper, .silent = TRUE)    # IDIOMATIC 2
#
# # `trying()` creates an adverb, and `$.tag_adverb` is a shortcut so
# # trying()$paste is equivalent to trying()(paste) (see example below)
# trying()$paste("hello",world, .else = toupper, .silent = TRUE)
# trying()(paste)("hello",world, .else = toupper, .silent = TRUE)
# trying()(paste, .silent = TRUE)("hello",world, .else = toupper)
# trying()(paste, .silent = TRUE, .else = toupper)("hello",world) # ALL IN ADVERB
# trying(.silent = TRUE)$paste("hello",world, .else = toupper)
# trying(.silent = TRUE)(paste)("hello",world, .else = toupper)
# trying(.silent = TRUE)(paste, .else = toupper)("hello",world)
# trying(.silent = TRUE, .else = toupper)$paste("hello",world)    # IDIOMATIC 1
# trying(.silent = TRUE, .else = toupper)(paste)("hello",world)   # THE CANONICAL ONE, START THERE
# # doesn't work and it's normal, the expression `try_else` is a tag,
# # it doesn't take a function as an input (`paste` is fed to `.else` !)
# trying(paste)("hello",world, .else = toupper,.silent = TRUE)
#
# identity_tag <- tag(eval(CALL))
# identity_tag$mean(1:3)
# trying <- tag(try(eval(CALL), silent = TRUE))
# trying$log(10)
# trying$log("a")
