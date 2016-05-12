process_conditions <- function(l, env = parent.frame()) {
    len <- length(l)
    if (len == 0) return(NULL)
    newlist <- vector("list", len)
    for (i in seq_along(newlist)) {
        where <- l[[i]]
        newlist[[i]] <- partial_sub(where, env = env)
    }
    newlist
}
