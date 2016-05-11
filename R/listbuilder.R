#' @export
as.listbuilder <- function(...) UseMethod("as.listbuilder")

#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @export
as.listbuilder.character <- function(x, id_type) {
    assert_that(is.string(x))
    structure(list(query = x, id_type = id_type),
              class = c("rawqry", "listbuilder"))
}

#' @export
print.listbuilder <- function(x) cat("Listbuilder object")
