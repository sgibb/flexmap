# "constant" non exported named vector for the array dimensions
.dim <- c(feature=1L, sample=2L, dilution=3L, replicate=4L)
.dimnames <- function()names(.dim)
