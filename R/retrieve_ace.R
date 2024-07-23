#' Extrait les consultations externes à l'hôpital.
#'
#' - Sélectionne les codes prestations (ACT_COD) parmi : C et CS.
#' - TODO: Implémente des filtres qualités sur les codes retours
#'
#' @param start_date Date de début de la période
#' @param end_date Date de fin de la période
#' @param spe_codes Codes spécialités
#' @param ben_table_name Nom de la table BEN
#' @param output_table_name Nom de la table de sortie
#' @param r_output_path Chemin de sauvegarde des données R
#'
#' @return Tables MCO_FCSTC||MCO_CSC pour une année donnée
#'
#' @export
extract_hospital_consultations <- function(
    start_date = NULL,
    end_date = NULL,
    spe_codes = NULL,
    ben_table_name = NULL,
    output_table_name = NULL,
    conn = NULL) {
  if (is.null((conn))) {
    conn <- initialize_connection() # Connect to database
  }

  consultation_act_codes <- c("C", "CS")

  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  formatted_start_date <- format(start_date, "%Y-%m-%d")
  formatted_end_date <- format(end_date, "%Y-%m-%d")
  typed_start_date <- as.Date(start_date)
  typed_end_date <- as.Date(end_date)

  for (year in start_year:end_year) {
    start_time <- Sys.time()
    print(glue::glue("Processing year: {year}"))
    formatted_year <- sprintf("%02d", year %% 100)

    ben <- dplyr::tbl(conn, ben_table_name)
    cstc <- dplyr::tbl(conn, glue::glue("T_MCO{formatted_year}CSTC"))
    fcstc <- dplyr::tbl(conn, glue::glue("T_MCO{formatted_year}FCSTC"))

    fcstc <- fcstc %>%
      select(ETA_NUM, SEQ_NUM, ACT_COD, EXE_SPE) %>%
      distinct()

    date_condition <- glue::glue(
      "EXE_SOI_DTD <= DATE '{formatted_end_date}' AND EXE_SOI_DTD >= DATE '{formatted_start_date}'"
    )
    ace <- cstc %>%
      select(ETA_NUM, SEQ_NUM, NIR_ANO_17, EXE_SOI_DTD) %>%
      distinct() %>%
      filter(sql(date_condition))
    ace <- ace %>%
      left_join(fcstc, by = c("ETA_NUM", "SEQ_NUM")) %>%
      select(NIR_ANO_17, EXE_SOI_DTD, ACT_COD, EXE_SPE) %>%
      filter(ACT_COD %in% consultation_act_codes) %>%
      distinct()

    if (!is.null(spe_codes)) {
      ace <- ace %>%
        filter(EXE_SPE %in% spe_codes)
    }

    ben <- ben %>%
      select(BEN_IDT_ANO, BEN_NIR_PSA) %>%
      distinct()

    query <- ben %>%
      inner_join(ace, by = c("BEN_NIR_PSA" = "NIR_ANO_17")) %>%
      select(BEN_IDT_ANO, EXE_SOI_DTD, ACT_COD, EXE_SPE) %>%
      distinct()

    create_table_or_insert_from_query(conn = conn, output_table_name = output_table_name, query = query)

    end_time <- Sys.time()
    print(glue::glue("Time taken for year {year}: {round(difftime(end_time, start_time, units='mins'),1)} mins."))
  }

  query <- dplyr::tbl(conn, output_table_name)
  consultations <- collect(query)

  DBI::dbDisconnect(conn)
  return(consultations)
}
