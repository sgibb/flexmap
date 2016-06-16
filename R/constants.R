# "constant" non exported named vector for the array dimensions
.dim <- c(sample=1L, feature=2L, dilution=3L, replicate=4L)
.dimnames <- function()names(.dim)
