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
    getcdw::get_cdw(query, ...)
}
