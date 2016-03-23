#' @export
partial_sub <- function(expr, env = parent.frame()) {
    env = as.list(env)
    eval(substitute(substitute(e, env), list(e = expr)))
}
