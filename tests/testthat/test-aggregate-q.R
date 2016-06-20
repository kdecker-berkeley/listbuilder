context("aggregate_q")
agq <- aggregate_q_(table = "f_transaction_detail_mv",
                    where = list(quote(alloc_school_code %in% ("HSB")),
                                 quote(between(giving_record_dt,
                                               to_date(20140101L, "yyyymmdd"),
                                               to_date(20141231L, "yyyymmdd")))),
                    having = list(quote(sum(benefit_aog_credited_amt) >= 1000)),
                    id_field = "donor_entity_id_nbr",
                    id_type = "entity_id",
                    schema = "CDW")

test_that("aggregate_q_ generates all expected components", {

    # did we successfully generate a valid listbuilder object?
    # does it have the expected pieces
    expect_is(agq, "listbuilder")
    expect_equal(length(agq$id_field), 1L)
    expect_equal(length(agq$id_type), 1L)
    expect_equal(length(agq$schema), 1L)

    expect_equal(agq$table, "f_transaction_detail_mv")
    expect_equal(agq$id_field, "donor_entity_id_nbr")
    expect_equal(agq$id_type, "entity_id")
    expect_equal(agq$schema, "CDW")

    # fed in two where conditions and one having condition,
    # so expect the same out
    expect_equal(length(agq$where), 2L)
    expect_equal(length(agq$having), 1L)

    # one of the where conditions should be a call to `%in%`
    # another one should be a call to between()
    where_operators <- lapply(agq$where, "[[", 1)
    where_should_be <- list(quote(`%in%`), quote(between))
    expect_identical(setdiff(where_operators, where_should_be), list())
    expect_identical(setdiff(where_should_be, where_operators), list())

    # the having operator should be `>=`
    having_operator <- lapply(agq$having, "[[", 1)
    having_should_be <- list(quote(`>=`))
    expect_identical(setdiff(having_operator, having_should_be), list())
    expect_identical(setdiff(having_should_be, having_operator), list())

    having_comparison <- agq$having[[1]][[3]]
    expect_equal(having_comparison, 1000)
})

test_that("aggregate_q_ results in valid query", {
    expect_is(get_cdw(agq), "data.frame")
})
