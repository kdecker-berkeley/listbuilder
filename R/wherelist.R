wherelist <- function(l, env = parent.frame()) {
    len <- length(l)
    if (len == 0) return(list())
    newlist <- vector("list", len)
    for (i in 1:len) {
        where <- l[[i]]
        newlist[[i]] <- partial_sub(where, env = env)
    }
    newlist
}
