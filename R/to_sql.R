#' Translate a listbuilder object into SQL
#'
#' @param obj the listbuilder object to be converted
#' @export
to_sql <- function(obj) UseMethod("to_sql")

#' @export
to_sql.rawqry <- function(lb) {
    as.character(lb$query)
}

#' @export
to_sql.listbuilder <- function(lb) {
    if (!is_atomic(lb)) return(to_sql_compound(lb))
    if (is_flist(lb)) return(to_sql_flist(lb))
    stop("Don't know how to deal with atomic objects of class ", class(lb))
}

#' @importFrom whisker whisker.render
#' @export
to_sql.simple_q <- function(lb) {
    template <- simple_q_template()
    haswhere <- length(lb$where) > 0
    where <- paste(lb$where, collapse = " and ")
    where <- unquote(where)
    whisker.render(template,
                   data = list(table = lb$table,
                               haswhere = haswhere,
                               where = where,
                               id_field = lb$id_field,
                               id_type = lb$id_type,
                               schema = lb$schema))
}

#' @importFrom whisker whisker.render
#' @export
to_sql_flist <- function(lb) {
    template <- flist_template()
    original_query <- to_sql(get_rhs(lb))
    haswhere <- length(lb$where) > 0
    where <- paste(lb$where, collapse = " and ")
    where <- unquote(where)
    whisker.render(template,
                   data = list(
                       id_type = get_id_type(lb),
                       table = lb$table,
                       from = lb$from,
                       to = lb$to,
                       haswhere = haswhere,
                       where = where,
                       original_query = original_query,
                       schema = lb$schema
                   ))
}

#' @importFrom whisker whisker.render
to_sql_compound <- function(lb) {
    operator <- get_operator(lb)
    block1 <- to_sql(get_lhs(lb))
    block2 <- to_sql(get_rhs(lb))

    template <- lb_compound_template()
    whisker.render(template,
                   data = list(block1 = block1,
                               block2 = block2,
                               operator = operator))

}
