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
#' @param savedlist a listbuilder list or report
#' @param table the table within which the flist will happen
#' @param from the origin ID field
#' @param to the destination ID field
#' @param id_type the id_type of the new listbuilder object
#' @param where any pre-aggregation conditions (lazy_dots)
#' @param having any post-aggregation conditions (lazy_dots)
#' @param schema "CDW" by default, no need to change it
#' @param env enviornment for evaluating table, from, to, and ...
#' @export
flist_ <- function(savedlist, table, from, to, id_type,
                   where = NULL, having = NULL, schema) UseMethod("flist_")

#' @export
flist_.listbuilder <- function(savedlist, table, from, to, id_type,
                               where = NULL, having = NULL, schema) {
    where <- process_conditions(where)
    having <- process_conditions(having)

    structure(list(lhs = NULL,
                   rhs = savedlist,
                   operator = "flist",
                   id_type = id_type,
                   from = from,
                   to = to,
                   table = table,
                   where = where,
                   having = having,
                   schema = schema),
              class = "listbuilder")
}
