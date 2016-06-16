get_table <- function(lb) lb$table
get_id_field <- function(lb) if (is_flist(lb)) lb$to else lb$id_field
get_schema <- function(lb) lb$schema
get_where <- function(lb) lb$where
get_having <- function(lb) lb$having

stringify <- function(conds) {
    if (is.null(conds)) return("")
    vapply(lapply(conds, r2sql), as.character, FUN.VALUE = character(1))
}

to_list <- function(lb) {
    if (is_atomic(lb) && !is_flist(lb))
        return(list(
            type = "atomic",
            table = get_table(lb),
            id_field = get_id_field(lb),
            id_type = get_id_type(lb),
            where = stringify(get_where(lb)),
            having = stringify(get_having(lb)),
            schema = get_schema(lb)
        ))

    if (is_flist(lb))
        return(list(
            type = "flist",
            table = get_table(lb),
            id_type = get_id_type(lb),
            from = lb$from,
            to = lb$to,
            where = stringify(get_where(lb)),
            having = stringify(get_having(lb)),
            schema = get_schema(lb),
            inner = to_list(lb$rhs)
        ))

    list(
        type = "compound",
        op = get_operator(lb),
        left = to_list(get_lhs(lb)),
        right = to_list(get_rhs(lb))
    )
}

#' @export
print.listbuilder <- function(lb, ...) {
    cat("LISTBUILDER DEFINITION (type: ", get_id_type(lb), ")", sep = "")
    listed <- to_list(lb)

    printout <- function(x) {
        if (x$type == "atomic") {
            conditions <- c(x$where, x$having)
            conditions <- Filter(function(conds) conds != "", conditions)
            return(list(
                source = paste(x$table, ".", x$id_field,
                               " (", x$id_type, ")", sep = ""),
                logic = conditions
            ))
        }

        if (x$type == "flist") {
            conditions <- c(x$where, x$having)
            conditions <- Filter(function(conds) conds != "", conditions)
            return(list(
                source = paste(x$table, ".", x$to,
                               " (", x$id_type, ")",
                               " via ", x$from,
                               sep = ""),
                logic = conditions,
                via = printout(x$inner)
            ))
        }

        opid <- make_op_id()

        res <- list(
            operator = x$op,
            left = printout(x$left),
            right = printout(x$right)
        )

        names(res) <- paste(names(res), " (", opid, ")", sep = "")
        res
    }

    output <- printout(listed)
    output <- jsonlite::toJSON(output, auto_unbox = TRUE)
    output <- jsonlite::prettify(output, indent = 4)
    output <- gsub("\\[|\\]|\\{|\\},?", "", unquote(output))
    output <- gsub(",\\s*\n", "\n", output)
    output <- gsub("\n\n", "\n", output)
    output <- gsub("\n\\s+\n", "\n", output)
    output <- gsub("    ", ".   ", output)
    cat(output)
    invisible(lb)
}

make_op_id <- function(seed = NULL) {
    if (!is.null(seed)) set.seed(seed)

    # two upper case letters to help visually identify the start of an id
    part0 <- paste(sample(LETTERS, 2), collapse = "")

    # start with three lower case letters
    #part1 <- paste(sample(letters, 3), collapse = "")
    part1 <- ""

    # then add two digits
    part2 <- paste(sample(0:9, 2), collapse = "")

    # then add two more lower case letters
    #part3 <- paste(sample(letters, 2), collapse = "")
    part3 <- ""

    paste0(part0, part1, part2, part3)[[1]]
}
