#' @export
get_id_type <- function(lb) UseMethod("get_id_type")

#' @export
get_id_type.listbuilder <- function(lb) lb$id_type
