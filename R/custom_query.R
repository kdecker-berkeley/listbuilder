#' Flexible template for queries with single-column results
#'
#' This functions provides a shorthand for queries of the form
#' "select x from (standalone-query) where ... group by ... having ..."
#'
#' This function is similar to aggregate_q except
#' the source is not constrained to being a single table, it can be any query.
#'
#' @param custom A SQL query (as a character vector of length 1)
#' @param id_field the name of the id column in \code{custom}
#' @param id_type a text label for the type of id
#' @param where pre-aggregated filter conditions
#' @param having post-aggregated filter conditions
#'
#' @export
custom_query <- function(custom, id_field, id_type,
                         where = NULL, having = NULL) {
    where <- process_conditions(where)
    having <- process_conditions(having)

    x <- list(custom = custom,
              table = "custom",
              id_field = id_field,
              id_type = id_type,
              where = where,
              having = having,
              schema = "custom")

    structure(x, class = c("custom_q", "listbuilder"))
}
