#' @export
print.report <- function(report, ...) {
    cat("Constituency definition (listbuilder)\n")
    print(get_listbuilder(report))
    q <- get_query(report)
    if (length(q) == 0) return(invisible(report))
    if (length(q) > 1)
        cat("queries:\n")
    else cat("query:\n")

    lapply(q, function(chunk) cat(chunk, "\n"))
    invisible(report)
}

get_listbuilder <- function(report) UseMethod("get_listbuilder")
get_template <- function(report) UseMethod("get_template")
get_query <- function(report) UseMethod("get_query")

#' @export
add_template <- function(report, template) UseMethod("add_template")

#' @export
get_id_type.report <- function(report) get_id_type(report$listbuilder)
get_query.report <- function(report) get_query(get_template(report))
get_query.NULL <- function(template) NULL

#' @export
report <- function(listbuilder, template = NULL) {
    if (!inherits(listbuilder, "listbuilder"))
        stop("invalid listbuilder object", call. = FALSE)

    if (!is.null(template)) {
        template <- as.report_template(template)
        check_id_types(listbuilder, template)
    }

    structure(
        list(
            listbuilder = listbuilder,
            template = template
        ),
        class = "report"
    )
}

get_listbuilder.report <- function(report) report$listbuilder
get_template.report <- function(report) report$template

#' @export
add_template.report <- function(report, template) {
    if (is.null(get_template(report))) {
        new_template <- as.report_template(template)
        if (get_id_type(new_template) != get_id_type(report))
            stop("Id type mismatch: ", get_id_type(report), " != ", get_id_type(new_template))
    }
    else new_template <- add_template(get_template(report), template)
    structure(
        list(
            listbuilder = get_listbuilder(report),
            template = new_template
        ),
        class = "report"
    )
}

#' @export
add_template.listbuilder <- function(lb, template) {
    report(lb, template = template)
}

#' @export
operate.report <- function(report, block2, operator) {
    newlb <- operate(get_listbuilder(report), block2, operator)

    report(newlb, get_template(report))
}

#' @export
flist_.report <- function(report, table, from, to, id_type,
                          where = NULL, having = NULL, schema) {
    lb <- get_listbuilder(report)
    newlb <- flist_(lb, table = table, from = from, to = to,
                    id_type = id_type, where = where, having = having,
                    schema = schema)
    report(newlb, get_template(report))
}

#' @export
to_sql.report <- function(report) {
    report_template <- lb_report_template()
    listbuilder <- get_listbuilder(report)
    id_type <- get_id_type(report)
    queries <- get_query(report)

    if (is.null(queries)) return(to_sql(listbuilder))

    chunknames <- paste("chunk", seq_along(queries), sep = "")

    chunks <- Map(function(query, chunkname) {
        list(chunkname = chunkname,
             chunk = query)
    }, queries, chunknames)

    querydata <- list(
        listbuilder = to_sql(listbuilder),
        id_type = get_id_type(report),
        chunks = if (length(chunks) > 0) chunks else NULL
    )

    whisker.render(template = report_template,
                   data = querydata)

}

lb_report_template <- function() {
"
with listylisty as (
{{{listbuilder}}}
)

{{#chunks}}
,{{chunkname}} as (
{{chunk}}
)
{{/chunks}}

select *
from
listylisty
{{#chunks}}
left join {{chunkname}}
using ({{id_type}})
{{/chunks}}
"
}
