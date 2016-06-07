#' Translate a listbuilder object into SQL
#'
#' @param obj the listbuilder object to be converted
#' @export
to_sql <- function(obj) {
    id_type <- get_id_type(obj)
    query <- as_sql(obj)
    paste0("select distinct ", id_type, " from (",
           query, ")")
}

as_sql <- function(obj) UseMethod("as_sql")

as_sql.rawqry <- function(lb) {
    as.character(lb$query)
}

as_sql.listbuilder <- function(lb) {
    if (!is_atomic(lb)) return(as_sql_compound(lb))
    if (is_flist(lb)) return(as_sql_flist(lb))
    stop("Don't know how to deal with atomic objects of class ", class(lb))
}

#' @importFrom whisker whisker.render
as_sql.aggregate_q <- function(lb) {
    # load template
    template <- aggregate_q_template()

    # convert where conditions to sql strings
    where <- lb$where
    if (!is.null(where))
        where <- lapply(where, function(x) r2sql(x))

    haswhere <- length(where) > 0

    where <- paste(where, collapse = " and ")
    where <- unquote(where)

    # convert having conditions into sql strings (note similarity to where conds)
    having <- lb$having
    if (!is.null(having))
        having <- lapply(having, function(x) r2sql(x))

    hashaving <- length(having) > 0
    having <- paste(having, collapse = " and ")
    having <- unquote(having)

    # render template
    whisker.render(template,
                   data = list(table = lb$table,
                               haswhere = haswhere,
                               where = where,
                               hashaving = hashaving,
                               having = having,
                               id_field = lb$id_field,
                               id_type = lb$id_type,
                               schema = lb$schema))
}



#' @importFrom whisker whisker.render
as_sql_flist <- function(lb) {
    # load template
    template <- flist_template()

    # the subquery to be flisted
    original_query <- as_sql(get_rhs(lb))

    # convert where conditions to sql strings
    where <- lb$where
    if (!is.null(where))
        where <- lapply(where, function(x) r2sql(x))

    haswhere <- length(where) > 0
    where <- paste(where, collapse = " and ")
    where <- unquote(where)

    # convert having conditions to sql strings
    having <- lb$having
    if (!is.null(having))
        having <- lapply(having, function(x) r2sql(x))

    hashaving <- length(having) > 0
    having <- paste(having, collapse = " and ")
    having <- unquote(having)

    # render the template
    whisker.render(template,
                   data = list(
                       id_type = get_id_type(lb),
                       table = lb$table,
                       from = lb$from,
                       to = lb$to,
                       haswhere = haswhere,
                       where = where,
                       hashaving = hashaving,
                       having = having,
                       original_query = original_query,
                       schema = lb$schema
                   ))
}

#' @importFrom whisker whisker.render
as_sql_compound <- function(lb) {
    operator <- get_operator(lb)
    block1 <- as_sql(get_lhs(lb))
    block2 <- as_sql(get_rhs(lb))

    template <- lb_compound_template()
    whisker.render(template,
                   data = list(block1 = block1,
                               block2 = block2,
                               operator = operator))

}
