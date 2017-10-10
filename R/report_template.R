get_query.report_template <- function(template) template$query
get_column_formats <- function(template) UseMethod("get_column_formats")
get_column_formats.report_template <- function(template) attr(template, "column_formats")
get_column_formats.report <- function(template) get_column_formats(get_template(template))
get_column_formats.NULL <- function(template) NULL

as.report_template <- function(template, column_formats = NULL) UseMethod("as.report_template")

as.report_template.report_template <- function(template, column_formats = NULL) template

as.report_template.character <- function(template, column_formats = NULL) {
    assertthat::assert_that(assertthat::is.string(template))
    info <- extract_id_type(template)
    structure(
        list(
            query = list(info$query),
            id_type = info$id_type
        ),
        column_formats = column_formats,
        class = "report_template"
    )
}

as.report_template.connection <- function(con, column_formats = NULL) {
    template <- paste(readLines(con), collapse = "\n")
    as.report_template(template, column_formats = column_formats)
}

as.report_template.default <- function(template, column_formats = NULL)
    stop("cannot convert input to a report_template", call. = FALSE)

add_template.report_template <- function(template, newtemplate, column_formats = NULL) {
    assertthat::assert_that(assertthat::is.string(newtemplate))
    info <- extract_id_type(newtemplate)
    if (info$id_type != get_id_type(template))
        stop("Tried to add a template with a different id_type to an existing template",
             call. = FALSE)
    lapply(get_query(template),
           function(tmpl) if (identical(tmpl, info$query))
               stop("Can't add the same output chunk twice", call. = FALSE))
    structure(
        list(
            query = c(template$query, list(info$query)),
            id_type = get_id_type(template)
        ),
        column_formats = c(get_column_formats(template), column_formats),
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
    if (length(id_type) < 1L)
        stop("A template must have exactly one field ##highlighted##, but yours has 0",
             call. = FALSE)
    if (length(id_type) > 1L)
        stop("Template can only have one field highlighted, but yours has ",
             length(id_type), ":\n",
             paste0(id_type, collapse = ", "),
             call. = FALSE)
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
