context("custom_query")
cq <- custom_query(
    custom = "
        select entity_id, report_name from cdw.d_entity_mv
        where person_or_org = 'P' and record_status_code = 'A'
    ",
    id_field = "entity_id",
    id_type = "entity_id",
    where = list(
        quote(report_name %like% '%Shah, Tarak%')
    ))

test_that("custom_query gets things right", {

    res <- get_cdw(cq)
    expect_is(res, "data.frame")
    expect_true(640993 %in% res$entity_id)

})
