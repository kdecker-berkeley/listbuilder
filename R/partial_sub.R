prep_dots <- function(...) {
    lazyeval::lazy_dots(..., .follow_symbols = TRUE, .ignore_empty = FALSE)
}

partial_sub <- function(.dots) {
    .dots
}
