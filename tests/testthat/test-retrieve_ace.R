test_that("retrieve_ace works", {
  conn <- initialize_connection()

  fake_ben_table <- data.frame(
    BEN_IDT_ANO = c(1, 2, 3),
    BEN_NIR_PSA = c(11, 12, 13)
  )

  fake_cstc_table <- data.frame(
    ETA_NUM = c(20, 20, 20),
    SEQ_NUM = c(31, 32, 33),
    NIR_ANO_17 = c(11, 12, 13),
    EXE_SOI_DTD = as.Date(c("2019-01-10", "2019-01-02", "2019-01-03"))
  )
  fake_fcstc_table <- data.frame(
    ETA_NUM = c(20, 20, 20),
    SEQ_NUM = c(31, 32, 33),
    EXE_SPE = c("01", "22", "99"),
    ACT_COD = c("C", "CS", "C")
  )
  ben_table_name <- "PD_10_22_IDS"
  output_table_name <- "PD_ACE_19_22"
  DBI::dbWriteTable(conn, ben_table_name, fake_ben_table)
  DBI::dbWriteTable(conn, "T_MCO19CSTC", fake_cstc_table)
  DBI::dbWriteTable(conn, "T_MCO19FCSTC", fake_fcstc_table)

  start_date <- as.Date("01/01/2019", format = "%d/%m/%Y")
  end_date <- as.Date("31/12/2019", format = "%d/%m/%Y")
  spe_codes <- c("01", "22", "32", "34")

  consultations <- extract_hospital_consultations(
    start_date = start_date,
    end_date = end_date,
    spe_codes = spe_codes,
    ben_table_name = ben_table_name,
    output_table_name = "test_output",
    conn = conn
  )

  DBI::dbDisconnect(conn)
  expect_equal(consultations, structure(list(BEN_IDT_ANO = c(
    1,
    2
  ), EXE_SOI_DTD = as.Date(c("2019-01-10", "2019-01-02")), ACT_COD = c(
    "C",
    "CS"
  ), EXE_SPE = c("01", "22")), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -2L)))
})
