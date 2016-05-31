#' Template for simple single-table queries.
#'
#' This functions provides a shorthand for queries of the form "select x from y where ..."
#'
#' @param table full name of the CDW table (eg d_entity_mv)
#' @param ... filter conditions
#' @param id_field the name of the id column in \code{table}
#' @param id_type a text label for the type of id
#' @param schema the schema
#' @param env evaluation environment
#'
#' @export
simple_q <- function(table, ..., id_field, id_type, schema) {
    where <- prep_dots(...)
    simple_q_(table = table,
              where = where,
              id_field = id_field,
              id_type = id_type,
              schema = schema)
}

#' @rdname simple_q
#' @export
simple_q_ <- function(table, where = NULL, id_field, id_type, schema) {
    aggregate_q_(table = table,
                 where = where,
                 having = NULL,
                 id_field = id_field,
                 id_type = id_type,
                 schema = schema)
}
