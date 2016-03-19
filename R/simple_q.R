unquote <- function(string) {
    gsub("\"", "", string)
}

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
#' @importFrom pryr dots
#' @export
simple_q <- function(table, ..., id_field = "entity_id",
                     id_type = "cads_id", schema = "CDW",
                     env = parent.frame()) {
    table <- deparse(partial_sub(substitute(table), env = env))
    table <- unquote(table)
    where <- pryr::dots(...)
    simple_q_(table, where, id_field, id_type, schema, env)
}

#' @rdname simple_q
simple_q_ <- function(table, where,
                      id_field = "entity_id", id_type = "cads_id",
                      schema = "CDW",
                      env = parent.frame()) {
    where <- wherelist(where, env = env)
    where <- lapply(where, function(x) r2sql((list(x))))
    x <- list(table = table, where = where,
              id_field = id_field, id_type = id_type,
              schema = schema)
    structure(x, class = c("simple_q", "listbuilder"))
}
