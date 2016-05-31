r2sql <- function(x) {
    dplyr::translate_sql_(x, window = FALSE)
}
