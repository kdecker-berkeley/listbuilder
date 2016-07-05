context("flist")

query1 <- simple_q_("sf_entity_aog_summary_mv",
                    list(quote(summary_aog_code == 'SHSB'),
                         quote(raised_aog_amt > 0)),
                    id_field = "entity_id",
                    id_type = "entity_id",
                    schema = "CDW")

fq <- flist_(query1,
             table = "d_entity_mv",
             from = "entity_id",
             to = "hh_corp_entity_id",
             id_type = "entity_id",
             schema = "CDW")

fq2 <- flist_(query1,
              table = "d_entity_mv",
              from = "entity_id",
              to = "hh_corp_entity_id",
              id_type = "entity_id",
              having = quote(count(entity_id) > 1),
              schema = "CDW")


test_that("flist_ generates all expected components", {

    # did we successfully generate a valid listbuilder object?
    # does it have the expected pieces
    expect_is(fq, "listbuilder")

    expect_equal(fq$operator, "flist")
    expect_equal(fq$id_type, "entity_id")
    expect_equal(fq$from, "entity_id")
    expect_equal(fq$to, "hh_corp_entity_id")
    expect_equal(fq$table, "d_entity_mv")
    expect_equal(fq$schema, "CDW")

    expect_null(fq$lhs)
    expect_identical(fq$rhs, query1)
})

test_that("flist_ results in valid query", {
    expect_is(get_cdw(fq), "data.frame")
    expect_is(get_cdw(fq2), "data.frame")
})
