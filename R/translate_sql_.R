translate_sql_ <- function(dots, con = NULL, vars_group = NULL, vars_order = NULL,
                           vars_frame = NULL, window = TRUE, context = list())
{
    if (length(dots) == 0) {
        return(sql())
    }
    stopifnot(is.list(dots))
    if (!any(rlang::have_name(dots))) {
        names(dots) <- NULL
    }
    old_con <- dbplyr:::set_current_con(con)
    on.exit(dbplyr:::set_current_con(old_con), add = TRUE)
    if (length(context) > 0) {
        old_context <- dbplyr:::set_current_context(context)
        on.exit(dbplyr:::set_current_context(old_context), add = TRUE)
    }
    if (window) {
        old_group <- dbplyr:::set_win_current_group(vars_group)
        on.exit(dbplyr:::set_win_current_group(old_group), add = TRUE)
        old_order <- dbplyr:::set_win_current_order(vars_order)
        on.exit(dbplyr:::set_win_current_order(old_order), add = TRUE)
        old_frame <- dbplyr:::set_win_current_frame(vars_frame)
        on.exit(dbplyr:::set_win_current_frame(old_frame), add = TRUE)
    }
    variant <- dbplyr:::dbplyr_sql_translation(con)
    pieces <- lapply(dots, function(x) {
        if (rlang::is_null(rlang::get_expr(x))) {
            NULL
        }
        else if (rlang::is_atomic(rlang::get_expr(x))) {
            dbplyr::escape(get_expr(x), con = con)
        }
        else {
            mask <- sql_data_mask(x, variant, con = con, window = window)
            dbplyr::escape((rlang::eval_tidy(x, mask)), con = con)
        }
    })
    dbplyr::sql(unlist(pieces))
}
