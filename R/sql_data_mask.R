sql_data_mask <- function(expr, variant, con, window = FALSE,
                          strict = getOption("dplyr.strict_sql", FALSE)) {
    stopifnot(dbplyr:::is.sql_variant(variant))

    # Default for unknown functions
    unknown <- setdiff(dbplyr:::all_calls(expr), names(variant))
    op <- if (strict) dbplyr:::missing_op else dbplyr:::default_op
    expr2 <- expr
    names(expr2) <- row.names(expr2)
    top_env <- dbplyr:::ceply(unknown, op, parent = rlang::empty_env(), env = rlang::get_env(list2env(expr2)))


    # Known R -> SQL functions
    special_calls <- dbplyr:::copy_env(variant$scalar, parent = top_env)
    if (!window) {
        special_calls2 <- dbplyr:::copy_env(variant$aggregate, parent = special_calls)
    } else {
        special_calls2 <- dbplyr:::copy_env(variant$window, parent = special_calls)
    }
    special_calls2$`::` <- function(pkg, name) {
        pkg <- as.character(substitute(pkg))
        name <- as.character(substitute(name))
        if (!rlang::is_installed(pkg)) {
            cli::cli_abort("There is no package called {.pkg {pkg}}")
        }
        if (!rlang::env_has(rlang:::ns_env(pkg), name)) {
            cli::cli_abort("{.val {name}} is not an exported object from {.pkg {pkg}}")
        }

        if (rlang::env_has(special_calls2, name) || rlang::env_has(special_calls, name)) {
            rlang::env_get(special_calls2, name, inherit = TRUE)
        } else {
            # TODO use {.fun dbplyr::{fn}} after https://github.com/r-lib/cli/issues/422 is fixed
            cli::cli_abort("No known translation for `{pkg}::{name}()`")
        }
    }
    names <- dbplyr:::all_names(expr)
    idents <- lapply(names, dbplyr::ident)
    name_env <-  dbplyr:::ceply(idents, dbplyr::escape, con = con, parent = special_calls2)

    # Known sql expressions
    symbol_env <- rlang::env_clone(dbplyr:::base_symbols, parent = name_env)

    rlang::new_data_mask(symbol_env, top_env)

}
