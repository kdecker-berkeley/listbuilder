r2sql <- function(x) {
    if (packageVersion("dplyr") > "0.4.3") {
        dplyr::translate_sql_(x, window = FALSE)
    } else {
        dplyr::translate_sql_q(x, window = FALSE)
    }
}
