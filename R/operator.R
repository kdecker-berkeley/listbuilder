get_operator <- function(lb) UseMethod("get_operator")
get_operator.listbuilder <- function(lb) {
    lb$operator
}

get_lhs <- function(lb) {
    lb$lhs
}

get_rhs <- function(lb) {
    lb$rhs
}
