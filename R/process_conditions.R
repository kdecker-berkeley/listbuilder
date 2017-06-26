process_conditions <- function(conditions) {
    if (length(conditions) <= 0)  return(NULL)
    # partial_sub(conditions)
    if (!is.list(conditions)) return(list(conditions))
    conditions
}
