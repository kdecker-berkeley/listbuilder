#' Combine two lists via a set operator
#' @importFrom assertthat assert_that
#'
#' @param block1 a listbuilder list or a listbuilder report
#' @param block2 a listbuilder list
#' @param operator a combination operator, one of "intersect," "union," or "minus"
#'
#' @export
operate <- function(block1, block2, operator) UseMethod("operate")

#' @export
operate.listbuilder <- function(block1, block2, operator) {
    if (!inherits(block2, "listbuilder")) stop("Operation not defined")
    assert_that(identical(get_id_type(block1), get_id_type(block2)))
    operator <- check_operator(operator)

    structure(list(lhs = block1,
                   rhs = block2,
                   operator = operator,
                   id_type = get_id_type(block1)),
              class = "listbuilder")
}

check_operator <- function(operator) {
    stopifnot(operator %in% c("intersect", "union", "minus"))
    operator
}

#' @rdname operate
#' @export
`%and%` <- function(block1, block2) operate(block1, block2, "intersect")

#' @rdname operate
#' @export
`%or%` <- function(block1, block2) operate(block1, block2, "union")

#' @rdname operate
#' @export
`%minus%` <- function(block1, block2) operate(block1, block2, "minus")
