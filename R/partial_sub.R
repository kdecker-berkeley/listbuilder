#' @export
partial_sub <- function(expr, env = parent.frame()) {
    lazyeval::interp(expr, .values = env)
}
