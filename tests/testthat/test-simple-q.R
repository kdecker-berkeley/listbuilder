context("simple_q")

query1 <- simple_q_("sf_entity_aog_summary_mv",
                    pryr::dots(summary_aog_code == 'SHSB',
                               raised_aog_amt > 0),
                    id_field = "entity_id",
                    id_type = "entity_id",
                    schema = "CDW")

test_that("simple_q_ generates all expected components", {

    # did we successfully generate a valid listbuilder object?
    # does it have the expected pieces
    expect_is(query1, "listbuilder")
    expect_equal(length(query1$id_field), 1L)
    expect_equal(length(query1$id_type), 1L)
    expect_equal(length(query1$schema), 1L)

    expect_equal(query1$table, "sf_entity_aog_summary_mv")
    expect_equal(query1$id_field, "entity_id")
    expect_equal(query1$id_type, "entity_id")
    expect_equal(query1$schema, "CDW")

    # fed in two where conditions, so expect the same out
    expect_equal(length(query1$where), 2L)

    # one of the where conditions should be a call to `==`
    # another one should be a call to `>`
    where_operators <- lapply(query1$where, "[[", 1)
    where_should_be <- list(quote(`==`), quote(`>`))
    expect_identical(setdiff(where_operators, where_should_be), list())
    expect_identical(setdiff(where_should_be, where_operators), list())

    # did we get the right aog?
    aogcond <- Filter(function(x) x[[1]] == quote(`==`), query1$where)
    aogcond <- aogcond[[1]]
    aogcond <- aogcond[[length(aogcond)]]
    expect_identical(aogcond, "SHSB")
})

test_that("simple_q_ results in valid query", {
    expect_is(get_cdw(query1), "data.frame")
})
