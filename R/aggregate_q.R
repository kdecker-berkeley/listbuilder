#' Template for single-table aggregate queries
#'
#' This functions provides a shorthand for queries of the form
#' "select x from y where ... group by ... having ..."
#'
#' @param table full name of the CDW table (eg d_entity_mv)
#' @param id_field the name of the id column in \code{table}
#' @param id_type a text label for the type of id
#' @param where pre-aggregated filter conditions
#' @param having post-aggregated filter conditions
#' @param schema the schema
#' @param env evaluation environment
#'
#' @export
aggregate_q_ <- function(table, id_field, id_type,
                         where = NULL, having = NULL,
                         schema) {
    where <- process_conditions(where)
    having <- process_conditions(having)

    x <- list(table = table,
              id_field = id_field,
              id_type = id_type,
              where = where,
              having = having,
              schema = schema)

    structure(x, class = c("aggregate_q", "listbuilder"))
}
