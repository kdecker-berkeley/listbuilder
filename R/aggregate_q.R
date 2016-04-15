#' Template for single-table aggregate queries
#'
#' This functions provides a shorthand for queries of the form
#' "select x from y where ... group by ... having ..."
#'
#' @param table full name of the CDW table (eg d_entity_mv)
#' @param where pre-aggregated filter conditions
#' @param having post-aggregated filter conditions
#' @param id_field the name of the id column in \code{table}
#' @param id_type a text label for the type of id
#' @param schema the schema
#' @param env evaluation environment
#'
#' @export
aggregate_q_ <- function(table, where, having,
                         id_field = "entity_id", id_type = "entity_id",
                         schema = "CDW",
                         env = parent.frame()) {
    where <- wherelist(where, env = env)
    if (!is.null(where))
        where <- lapply(where, function(x) r2sql((list(x))))
    having <- wherelist(having, env = env)
    if (!is.null(having))
        having <- lapply(having, function(x) r2sql((list(x))))
    x <- list(table = table, where = where, having = having,
              id_field = id_field, id_type = id_type,
              schema = schema)
    structure(x, class = c("aggregate_q", "listbuilder"))
}
