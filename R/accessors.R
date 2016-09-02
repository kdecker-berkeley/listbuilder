#' @export
get_id_type <- function(lb) UseMethod("get_id_type")

#' @export
get_id_field <- function(lb) UseMethod("get_id_field")


#' @export
get_table <- function(lb) UseMethod("get_table")

#' @export
get_schema <- function(lb) UseMethod("get_schema")

#' @export
get_where <- function(lb) UseMethod("get_where")

#' @export
get_having <- function(lb) UseMethod("get_having")

#' @export
get_id_type.listbuilder <- function(lb) lb$id_type

#' @export
get_table.listbuilder <- function(lb) lb$table

#' @export
get_id_field.listbuilder <- function(lb) if (is_flist(lb)) lb$to else lb$id_field

#' @export
get_schema.listbuilder <- function(lb) lb$schema

#' @export
get_where.listbuilder <- function(lb) lb$where

#' @export
get_having.listbuilder <- function(lb) lb$having
