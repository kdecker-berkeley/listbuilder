sql_data_mask <- function(expr, variant, con, window = FALSE,
                          strict = getOption("dplyr.strict_sql", FALSE)) {
    stopifnot(dbplyr:::is.sql_variant(variant))

    # Default for unknown functions
    unknown <- setdiff(all_calls(expr), names(variant))
    op <- if (strict) missing_op else default_op
    top_env <- ceply(unknown, op, parent = rlang::empty_env(), env = get_env(expr))

    # Known R -> SQL functions
    special_calls <- copy_env(variant$scalar, parent = top_env)
    if (!window) {
        special_calls2 <- copy_env(variant$aggregate, parent = special_calls)
    } else {
        special_calls2 <- copy_env(variant$window, parent = special_calls)
    }
    special_calls2$`::` <- function(pkg, name) {
        pkg <- as.character(substitute(pkg))
        name <- as.character(substitute(name))
        if (!is_installed(pkg)) {
            cli_abort("There is no package called {.pkg {pkg}}")
        }
        if (!env_has(ns_env(pkg), name)) {
            cli_abort("{.val {name}} is not an exported object from {.pkg {pkg}}")
        }

        if (env_has(special_calls2, name) || env_has(special_calls, name)) {
            env_get(special_calls2, name, inherit = TRUE)
        } else {
            # TODO use {.fun dbplyr::{fn}} after https://github.com/r-lib/cli/issues/422 is fixed
            cli_abort("No known translation for `{pkg}::{name}()`")
        }
    }
    names <- all_names(expr)
    idents <- lapply(names, ident)
    name_env <- ceply(idents, dbplyr::escape, con = con, parent = special_calls2)

    # Known sql expressions
    symbol_env <- rlang::env_clone(base_symbols, parent = name_env)

    rlang::new_data_mask(symbol_env, top_env)

}
