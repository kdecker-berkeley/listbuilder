get_query.report_template <- function(template) template$query

as.report_template <- function(template) UseMethod("as.report_template")

as.report_template.report_template <- function(template) template

as.report_template.character <- function(template) {
    assertthat::assert_that(assertthat::is.string(template))
    info <- extract_id_type(template)
    structure(
        list(
            query = list(info$query),
            id_type = info$id_type
        ),
        class = "report_template"
    )
}

as.report_template.connection <- function(con) {
    template <- paste(readLines(con), collapse = "\n")
    as.report_template(template)
}

as.report_template.default <- function(template)
    stop("cannot convert input to a report_template", call. = FALSE)

add_template.report_template <- function(template, newtemplate) {
    assertthat::assert_that(assertthat::is.string(newtemplate))
    info <- extract_id_type(newtemplate)
    if (info$id_type != get_id_type(template))
        stop("Tried to add a template with a different id_type to an existing template",
             call. = FALSE)
    structure(
        list(
            query = c(template$query, list(info$query)),
            id_type = get_id_type(template)
        ),
        class = "report_template"
    )
}

get_id_type.report_template <- function(template) {
    id_type <- template$id_type
    if (!assertthat::is.string(id_type))
        stop("there was a problem with the id_type of the template",
             call. = FALSE)
    id_type
}

extract_id_type <- function(template) {
    matches <- stringr::str_match_all(template, "##([^#]+)##")
    id_type = unique(matches[[1]][,2, drop = TRUE])
    if (!assertthat::is.string(id_type))
        stop("Found problems with the id types in your report template",
             call. = FALSE)
    query <- gsub("##([^#]+)##", "\\1", template)
    list(query = query, id_type = id_type)
}

check_id_types <- function(listbuilder, template) {
    stopifnot(inherits(listbuilder, "listbuilder"))
    stopifnot(inherits(template, "report_template"))
    lb_id <- get_id_type(listbuilder)
    tp_id <- get_id_type(template)
    if (lb_id != tp_id)
        stop("ids not equal: ", lb_id, " != ", tp_id, call. = FALSE)
    TRUE
}
