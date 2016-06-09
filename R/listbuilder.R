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

print.listbuilder <- function(x) {
    id <- get_id_type(x)
    cat("Listbuilder object (type: ", id, ")\n", sep = "")
    print_details(x)
}

print_details <- function(x) {
    if (!is_atomic(x)) {
        print_details(get_lhs(x))
        print_details(get_rhs(x))
        return()
    }
    conditions <- c(x$where, x$having)
    operators <- vapply(conditions, expression_op,
                        FUN.VALUE = character(1))
    fields <- vapply(conditions, expression_field,
                     FUN.VALUE = character(1))
    vals <- vapply(conditions, expression_filter,
                   FUN.VALUE = character(1))

    for (i in seq_along(operators)) {
        op <- operators[[i]]
        field <- fields[[i]]
        val <- vals[[i]]
        cat(field, ": ", val, sep = "")
    }
}

expression_op <- function(exp) {
    as.character(as.list(exp)[[1]])
}

expression_field <- function(exp) {
    as.character(as.list(exp)[[2]])
}

expression_filter <- function(exp) {
    as.character(as.list(exp)[[3]])
}
