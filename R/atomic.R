is_atomic <- function(lb) UseMethod("is_atomic")
is_atomic.listbuilder <- function(lb) {
    !get_operator(lb) %in% c("intersect", "union", "minus")
}

is_flist <- function(lb) UseMethod("is_flist")
is_flist.listbuilder <- function(lb) {
    get_operator(lb) == "flist"
}
