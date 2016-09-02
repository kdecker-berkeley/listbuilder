#' @export
idlist <- function(ids, id_type = "entity_id", quoted = F) {
    if (quoted) ids <- paste0("'", ids, "'")

    as_sql <- paste0(
        "select ", ids, " as ", id_type, " from dual",
        collapse = "\nunion\n"
    )
    as_sql <- paste0("(", as_sql, ")")

    f <- function(query)
        switch(
            query,
            id_type = id_type,
            id_field = id_type,
            as_sql = as_sql,
            table = "manual",
            having = NULL,
            where = as.call(list(quote(`%in%`), id_type, ids)),
            schema = NULL,
            ids = ids
        )
    structure(f, class = c("idlist", "listbuilder"))
}

as_sql.idlist <- function(ids) ids("as_sql")

#' @export
get_id_field.idlist <- function(ids) ids("id_field")

#' @export
get_id_type.idlist <- function(ids) ids("id_type")

#' @export
get_where.idlist <- function(ids) ids("where")

#' @export
get_having.idlist <- function(ids) ids("having")

#' @export
get_schema.idlist <- function(ids) ids("schema")

#' @export
get_table.idlist <- function(ids) ids("table")

#' @export
get_operator.idlist <- function(ids) NULL
