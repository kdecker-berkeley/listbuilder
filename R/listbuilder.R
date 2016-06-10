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
print.listbuilder <- function(x) {
    id <- get_id_type(x)
    cat("Listbuilder object (type: ", id, ")\n", sep = "")
    cat("=================================================", "\n")
    print_details(x)
}

print_details <- function(x) {
    if (!is_atomic(x)) {
        print_details(get_lhs(x))
        cat("\n", toupper(get_operator(x)), "\n\n", sep = "")
        print_details(get_rhs(x))
        return()
    }

    if (is_flist(x)) {
        print_details(get_rhs(x))
        cat(":", x$from, " --> ", x$to, " via ", x$table, "\n", sep = "")
        return()
    }
    conditions <- c(x$where, x$having)
    operators <- vapply(conditions, expression_op,
                        FUN.VALUE = character(1))
    fields <- vapply(conditions, expression_field,
                     FUN.VALUE = character(1))
    vals <- vapply(conditions, expression_filter,
                   FUN.VALUE = character(1))

    cat(":", x$table, ".", x$id_field, "\n", sep = "")

    for (i in seq_along(operators)) {
        op <- operators[[i]]
        field <- fields[[i]]
        val <- vals[[i]]
        if (op %in% c("%in%", "=="))
            cat("    ", field, ": ", val, "\n", sep = "")
        else cat("    ", field, "\n", sep = "")
    }
}

expression_op <- function(exp) {
    deparse(as.list(exp)[[1]])
}

expression_field <- function(exp) {
    deparse(as.list(exp)[[2]])
}

expression_filter <- function(exp) {
    deparse(as.list(exp)[[3]])
}
