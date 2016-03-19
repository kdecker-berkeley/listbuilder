#' @importFrom getcdw get_cdw
#' @export
get_cdw.listbuilder <- function(lb, dsn = "CDW2", uid = NULL, pwd = NULL,
                                stringsAsFactors = FALSE, ...) {
    query <- to_sql(lb)
    getcdw::get_cdw(query, dsn = dsn, uid = uid,
                    pwd = pwd, stringsAsFactors = stringsAsFactors, ...)
}
