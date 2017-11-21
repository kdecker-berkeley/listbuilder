context("reports")
lb <- aggregate_q_(table = "f_transaction_detail_mv",
                   where = list(quote(alloc_school_code %in% ("HSB")),
                                quote(between(giving_record_dt,
                                              to_date(20140101L, "yyyymmdd"),
                                              to_date(20141231L, "yyyymmdd")))),
                   having = list(quote(sum(benefit_aog_credited_amt) >= 1000)),
                   id_field = "donor_entity_id_nbr",
                   id_type = "entity_id",
                   schema = "CDW")

template1 <- "select ##entity_id##, report_name from cdw.d_entity_mv"
template2 <- "select donor_entity_id_nbr as ##entity_id##, sum(benefit_aog_credited_amt) as giving from cdw.f_transaction_detail_mv group by donor_entity_id_nbr"
template3 <- "select ##entity_id##, report_name from cdw.d_entity_mv where entity_id >= 100"

test_that("report results in valid query", {
    report0 <- report(lb)
    expect_is(get_cdw(report0), "data.frame")
    expect_identical(names(get_cdw(report0)), "entity_id")

    report0.5 <- add_template(lb, template1)
    expect_is(get_cdw(report0.5), "data.frame")
    expect_identical(names(get_cdw(report0.5)), c("entity_id", "report_name"))

    report1 <- report(lb, template1)
    expect_is(get_cdw(report1), "data.frame")
    expect_identical(names(get_cdw(report1)), c("entity_id", "report_name"))

    report1a <- add_template(report0, template1)
    expect_identical(report1a, report1)

    report2 <- add_template(report1, template2)
    expect_is(get_cdw(report2), "data.frame")
    expect_identical(names(get_cdw(report2)), c("entity_id", "report_name", "giving"))

    report3 <- add_template(report0, template3)
    expect_is(get_cdw(report3), "data.frame")
    expect_identical(names(get_cdw(report3)), "entity_id", "report_name")
})

test_that("can't add same chunk twice", {
    expect_error(add_template(add_template(lb, template1), template1), "same output")
    rpt <- add_template(lb, template1)
    rpt <- add_template(rpt, template2)
    expect_error(add_template(rpt, template1), "same output")

})

test_that("can specify column formats", {
    rpt <- add_template(
        lb,
        "select ##entity_id##, capacity_rating_code from cdw.d_entity_mv",
        column_formats = list(capacity_rating_code = as.integer))

    res <- get_cdw(rpt)
    expect_is(res$capacity_rating_code, "integer")
})

test_that("errors when column formats don't match columns", {
    rpt <- add_template(
        lb,
        "select ##entity_id##, capacity_rating_code from cdw.d_entity_mv",
        column_formats = list(capacity_rating = as.integer))
    expect_error(get_cdw(rpt), "column formats")
})

lb <- discoveryengine::has_capacity(1)

test_that("Can create a report", {
    expect_is(
        add_template(
            lb,
            "select ##entity_id##, report_name from cdw.d_entity_mv"
        ), "report")
})

test_that("Helpful error messages on mis-specified reports", {
    expect_error(
        add_template(
            lb,
            "select entity_id, report_name from cdw.d_entity_mv"
        ), "exactly one field")
    expect_error(
        add_template(
            lb,
            "select ##entity_id##, ##report_name## from cdw.d_entity_mv"
        ), "entity_id, report_name")
})
