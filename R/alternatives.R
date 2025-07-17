#' Find alternative products for all products containing certain active substances
#'
#' This function searches for uses of a given list of active substances and reports
#' either a table of uses with the number of available alternative products for each
#' use, a detailed table of the alternative product uses, a table of uses without
#' alternatives, or a list containing these three tables.
#'
#' A use is defined here as a combination of an application area, a crop
#' ('culture') and a pathogen ('pest'). This means, that for an alternative
#' product to be found, there has to be an exact match of application
#' area, crop an pathogen.
#'
#' @importFrom stringr str_detect str_replace
#' @importFrom rlang sym :=
#' @importFrom dplyr case_when mutate any_of
#' @param srppp A [srppp_dm] object.
#' @param active_ingredients Character vector of active ingredient names that
#'   will be matched against the column 'substances_de' in the srppp table
#'   'substances'.
#' @param details Should a table of alternative uses with 'wNbr' and 'use_nr' be
#'   returned?
#' @param missing If this is set to TRUE, uses without alternative product
#'   registrations are listed.
#' @param list If TRUE, a list of three tables is returned, a table of uses
#'   without alternative products ("Lückenindikationen"), a table of the number
#'   of alternative products for each use, if any, and a detailed table of all
#'   the alternative uses. This argument overrides the arguments 'details' and
#'   'missing'.
#' @param lang The language used for the active ingredient names and the
#'   returned tables.
#' @param resolve_cultures Logical. Specifies whether to resolve culture levels
#'   to their most specific hierarchical level (leaf nodes) using a parent-child
#'   relationship dataset derived from a culture tree.
#'   - If `TRUE` (default), the function maps culture levels to their corresponding
#'   leaf nodes. This enables precise identification of alternative products at
#'   the most specific culture level. This resolves the problem that products
#'   are sometimes authorised for different cultural groups. This means that
#'   actual "Lückenindikationen" can be identified. Only supported in German,
#'   i.e. if `lang = "de"`.
#'   - If `FALSE`, the function retains the original culture levels without
#'   hierarchical resolution. This option is useful when the original structure
#'   of the culture data needs to be preserved.
#'   **Note**: This argument is only applicable when the language is set to
#'   German (`de`). For other languages, the `resolve_cultures` functionality
#'   is not implemented and must be set to `FALSE`.
#'@return A [tibble] containing use definitions as defined above, i.e.
#'  containing columns with the application area, crop and pathogen. Depending
#'  on the arguments, columns summarizing or listing the alternative products
#'  and/or uses are also contained.
#' @export
#' @examples
#' \donttest{
#' sr <- try(srppp_dm())
#'
#' # Fall back to internal test data if downloading or reading fails
#' if (inherits(sr, "try-error")) {
#'   sr <- system.file("testdata/Daten_Pflanzenschutzmittelverzeichnis_2024-12-16.zip",
#'       package = "srppp") |>
#'     srppp_xml_get_from_path(from = "2024-12-16") |>
#'     srppp_dm()
#' }
#'
#' # Examples with two active substances
#' actives_de <- c("Lambda-Cyhalothrin", "Deltamethrin")
#' alternative_products(sr, actives_de)
#' alternative_products(sr, actives_de, resolve_cultures = FALSE)
#' alternative_products(sr, actives_de, missing = TRUE)
#' alternative_products(sr, actives_de, details = TRUE)
#' alternative_products(sr, actives_de, list = TRUE)
#'
#' # Examples resolving cultures
#' actives_de <- c("Spinetoram")
#' alternative_products(sr, actives_de, resolve_cultures = FALSE, list = TRUE)
#' alternative_products(sr, actives_de, resolve_cultures = TRUE, list = TRUE)
#'
#' actives_de <- c("Schalenwicklergranulose-Virus")
#' alternative_products(sr, actives_de, resolve_cultures = FALSE, list = TRUE)
#' alternative_products(sr, actives_de, resolve_cultures = TRUE, list = TRUE)
#'
#' actives_de <- c("Emamectinbenzoat")
#' alternative_products(sr, actives_de, resolve_cultures = FALSE, list = TRUE)
#' alternative_products(sr, actives_de, resolve_cultures = TRUE, list = TRUE)
#'
#' # Example in Italian
#' actives_it <- c("Lambda-Cialotrina", "Deltametrina")
#' alternative_products(sr, actives_it, lang = "it", resolve_cultures = FALSE)
#' }
alternative_products <- function(srppp, active_ingredients,
  details = FALSE, missing = FALSE, list = FALSE, lang = c("de", "fr", "it"),
  resolve_cultures = TRUE)
{
  lang = match.arg(lang)
  substance_column <- paste("substance", lang, sep = "_")
  selection_criteria = paste(c("application_area", "culture", "pest"), lang, sep = "_")

  culture_column <- paste("culture", lang, sep = "_")
  leaf_culture_column <- paste("leaf_culture", lang, sep = "_")
  pest_column <- paste("pest", lang, sep = "_")

  # Select products from the srppp containing the active ingredients in question
  affected_products <- srppp$substances |>
    filter(srppp$substances[[substance_column]] %in% active_ingredients) |>
    left_join(srppp$ingredients[c("pk", "pNbr")], by = "pk") |> # get P-Numbers
    left_join(srppp$products, by = "pNbr") |>
    select(c("pNbr", "wNbr")) |>
    arrange(pick(all_of(c("pNbr", "wNbr"))))

  # Select uses of the affected products
  affected_uses <- srppp$uses |>
    filter(pNbr %in% affected_products$pNbr)

  # Get unique combinations of application areas, cultures and pests for the affected uses
  affected_cultures_x_pests <- affected_uses |>
    left_join(srppp$cultures, by = c("pNbr", "use_nr"), relationship = "many-to-many") |>
    left_join(srppp$pests, by = c("pNbr", "use_nr"), relationship = "many-to-many") |>
    select(all_of(selection_criteria)) |>
    unique() |>
    arrange(pick(all_of(selection_criteria)))

  if (resolve_cultures == TRUE) {
    if (lang != "de") stop("Resolving cultures is only supported in German")
    affected_cultures_x_pests <- resolve_cultures(affected_cultures_x_pests, srppp)
    affected_cultures_x_pests <-
      affected_cultures_x_pests |>
      mutate(!!sym(culture_column) := !!sym(leaf_culture_column)) |>
      select(-all_of(leaf_culture_column), -any_of(paste0(culture_column, "_corrected")))
  }
  return_columns <- c("pNbr", "wNbr", "use_nr", "type", selection_criteria)

  # Select products without the active ingredients in question for the same pest(s)
  alternative_product_candidates <- srppp$products |>
    ungroup() |>
    filter(!srppp$products$wNbr %in% affected_products$wNbr)

  alternative_product_candidate_uses <- alternative_product_candidates |>
    left_join(srppp$uses, by = "pNbr", relationship = "many-to-many") |>
    left_join(srppp$cultures, by = c("pNbr", "use_nr"), relationship = "many-to-many") |>
    left_join(srppp$pests, by = c("pNbr", "use_nr"), relationship = "many-to-many") |>
    select(all_of(return_columns)) |>
    arrange(pick(all_of(return_columns))) |>
    filter(!!sym(pest_column) %in% affected_cultures_x_pests[[pest_column]])

  if (resolve_cultures == TRUE) {
    alternative_product_candidate_uses <- resolve_cultures(alternative_product_candidate_uses, srppp)
    alternative_product_candidate_uses <-
      alternative_product_candidate_uses |>
      mutate(!!sym(culture_column) := !!sym(leaf_culture_column)) |>  # Use dynamic leaf_culture column
      select(-all_of(leaf_culture_column), -any_of(paste0(culture_column, "_corrected")))
  }

  alternative_uses <- affected_cultures_x_pests |>
    left_join(alternative_product_candidate_uses,
              by = selection_criteria, relationship = "many-to-many")

  uses_without_alternatives <- alternative_uses |>
    filter(is.na(alternative_uses$pNbr)) |>
    select(all_of(selection_criteria))

  n_alternative_products <- alternative_uses |>
    select(all_of(c("wNbr", selection_criteria))) |>
    unique() |>
    group_by(pick(all_of(selection_criteria))) |>
    summarise(n_wNbr = sum(!is.na(wNbr)), .groups = "drop")

  n_alternative_product_types <- alternative_uses |>
    select(all_of(c("pNbr", selection_criteria))) |>
    unique() |>
    group_by(pick(all_of(selection_criteria))) |>
    summarise(n_pNbr = sum(!is.na(pNbr)), .groups = "drop")


  n_alternatives <- n_alternative_products |>
    left_join(n_alternative_product_types, by = selection_criteria)

  if (list) {
    ret <- list(
      uses_without_alternatives,
      n_alternatives,
      alternative_uses)
    names(ret) <- c(
      "No alternative",
      "Number of alternatives",
      "Alternative uses")
    return(ret)
  } else {
    if (details & missing) {
      stop("You cannot get details for missing alternatives")
    } else {
      if (missing) {
        return(uses_without_alternatives)
      } else {
        if (details) {
          return(alternative_uses)
        } else {
          return(n_alternatives)
        }
      }
    }
  }
}
