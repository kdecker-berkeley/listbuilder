#' @rdname people
#' @export
people_ <- function(.dots, include_deceased = FALSE, env = parent.frame()) {
    where <- wherelist(.dots, env = env)
    where <- c(where, quote(person_or_org == 'P'))
    if (!include_deceased)
        where <- c(where, quote(record_status_code == 'A'))
    else
        where <- c(where, quote(record_status_code %in% c('A', 'R', 'D')))
    where <- lapply(where, function(x) translate_sql_q((list(x))))
    simple_q_(table = "d_entity_mv",
              where = where, id_field = "entity_id",
              id_type = "cads_id", schema = "CDW")
}

#' Basic entity query
#'
#' @param ... any conditions
#' @param include_deceased should deceased individuals be included? (Defaults to FALSE)
#' @param env evaluation environment
#'
#' @importFrom pryr dots
#' @export
people <- function(..., include_deceased = FALSE, env = parent.frame()) {
    l <- pryr::dots(...)
    people_(l, include_deceased = include_deceased, env = env)
}
