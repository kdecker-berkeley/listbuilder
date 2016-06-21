#' @export
get_id_type <- function(lb) UseMethod("get_id_type")

#' @export
get_id_type.listbuilder <- function(lb) lb$id_type

#' @export
get_table <- function(lb) lb$table

#' @export
get_id_field <- function(lb) if (is_flist(lb)) lb$to else lb$id_field

#' @export
get_schema <- function(lb) lb$schema

#' @export
get_where <- function(lb) lb$where

#' @export
get_having <- function(lb) lb$having
