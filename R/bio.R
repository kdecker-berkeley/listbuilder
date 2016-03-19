#' @rdname bio
#' @export
bio_ <- function(tbl, .dots, env = parent.frame()) {
    where <- wherelist(.dots, env = env)
    where <- lapply(where, function(x) r2sql((list(x))))
    tbl <- paste("d_bio", tbl, "mv", sep = "_")
    simple_q_(table = tbl, where = where, id_field = "entity_id",
             id_type = "cads_id", schema = "CDW")

}

#' Simple query from biographic tables
#'
#' @param tbl the name of the bio table, e.g. degrees or activities
#' @param ... filter conditions
#' @param env symbol evaluation environment
#'
#' @export
bio <- function(tbl, ..., env = parent.frame()) {
    l <- pryr::dots(...)
    tbl <- deparse(partial_sub(substitute(tbl), env = env))
    tbl <- unquote(tbl)
    bio_(tbl, l, env)
}

wherelist <- function(l, env = parent.frame()) {
    len <- length(l)
    if (len == 0) return(list())
    newlist <- vector("list", len)
    for (i in 1:len) {
        where <- l[[i]]
        newlist[[i]] <- partial_sub(where, env = env)
    }
    newlist
}
