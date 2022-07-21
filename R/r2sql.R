r2sql <- function(x) {
    # if (packageVersion("dplyr") > "0.4.3") {
    #     outp <- dplyr::translate_sql_(x, window = FALSE)
    # } else {
    #     outp <- dplyr::translate_sql_q(x, window = FALSE)
    # }
    outp <- translate_sql_(x, window = FALSE, con = dbplyr:: simulate_dbi())
    outp = gsub("`", "", outp)
    unquote(outp)
}
