#' Convert a list of Ids to a new list of a possibly different type
#'
#' A list of allocations can be converted to a list of entities, via the
#' question "who gave to allocations in this list?" A list of entities can be
#' converted to a new list of entities via the question "Who are the children of
#' these entities?" And so on. \code{flist} is the general-purpose tool to make
#' these conversions.
#'
#' To use flist, you have to specify the origin and destination id fields, as
#' well as the "context" that defines the transformation. The "context" includes
#' the CDW table that contains both id fields, and any conditions. Example
#' context: \code{table = d_bio_relationship_mv},
#' condition = \code{relation_type_code \%in\% c("SF", "SM", "DF", "DM"}.
#' That would describe an output of IDs that are parents of the original ID list.
#'
#' @param savedlist a listbuilder list
#' @param table the table within which the flist will happen
#' @param from the origin ID field
#' @param to the destination ID field
#' @param ... any conditions, a set of boolean expressions
#' @param schema "CDW" by default, no need to change it
#' @param env enviornment for evaluating table, from, to, and ...
#'
#' @importFrom pryr dots
#' @export
flist <- function(savedlist, table, from, to, id_type = "entity_id",
                  ..., schema = "CDW", env = parent.frame()) {
    table <- deparse(partial_sub(substitute(table), env = env))
    table <- unquote(table)

    from <- deparse(partial_sub(substitute(from), env = env))
    from <- unquote(from)

    to <- deparse(partial_sub(substitute(to), env = env))
    to <- unquote(to)

    where <- pryr::dots(...)
    flist_(savedlist, table, from, to, id_type, where, schema, env)
}

#' @rdname flist
#' @export
flist_ <- function(savedlist, table, from, to, id_type = "entity_id",
                   .dots, schema = "CDW", env = parent.frame()) {
    where <- wherelist(.dots, env = env)
    where <- lapply(where, function(x) r2sql((list(x))))

    structure(list(lhs = NULL,
                   rhs = savedlist,
                   operator = "flist",
                   id_type = id_type,
                   from = from,
                   to = to,
                   table = table,
                   where = where,
                   schema = schema),
              class = "listbuilder")
}
