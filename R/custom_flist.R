#' @rdname flist_
#' @export
custom_flist <- function(savedlist, custom, from, to, id_type,
                         where = NULL, having = NULL) {
    where <- process_conditions(where)
    having <- process_conditions(having)

    structure(list(lhs = NULL,
                   rhs = savedlist,
                   operator = "flist",
                   id_type = id_type,
                   from = from,
                   to = to,
                   table = "custom",
                   custom = custom,
                   where = where,
                   having = having,
                   schema = "custom"),
              class = "listbuilder")
}
