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

test_that("report results in valid query", {
    report1 <- report(lb, template1)
    expect_is(get_cdw(report1), "data.frame")
    expect_identical(names(get_cdw(report1)), c("entity_id", "report_name"))

    report2 <- add_template(report1, template2)
    expect_is(get_cdw(report2), "data.frame")
    expect_identical(names(get_cdw(report2)), c("entity_id", "report_name", "giving"))
})
