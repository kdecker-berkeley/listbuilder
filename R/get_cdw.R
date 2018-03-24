#' @importFrom getcdw get_cdw
#' @export
getcdw::get_cdw

#' @export
get_cdw.listbuilder <- function(lb, ...) {
    query <- to_sql(lb)
    getcdw::get_cdw(query, ...)
}

#' @export
get_cdw.report <- function(report, ...) {
    query <- to_sql(report)
    res <- getcdw::get_cdw(query, ...)
    column_formats <- get_column_formats(report)
    reformat_columns(res, column_formats = column_formats)
}

reformat_columns <- function(res, column_formats) {
    if (length(column_formats) == 0L) return(res)

    # convert to lower case: sql case-insensitive and our database returns
    # all columns as uppercase, regardless of what the query says,
    # we convert everything to lower case b/c it's easier to look at. so
    # column formatter names (which have to match column names) should always
    # be lower case
    cols_to_modify <- tolower(names(column_formats))

    if (length(cols_to_modify[cols_to_modify != ""]) != length(column_formats))
        stop("All column_formats must be named")

    unidentified_cols <- !cols_to_modify %in% colnames(res)
    if (any(unidentified_cols))
        stop("Not all column formats understood: ", paste(cols_to_modify[unidentified_cols], collapse = ", "))

    for (i in seq_along(column_formats)) {
        res <- dplyr::mutate_at(
            res,
            .vars = cols_to_modify[[i]],
            .funs = column_formats[[i]]
        )
    }
    res
}
