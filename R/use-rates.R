#' Calculate application rates for active ingredients
#'
#' An application rate in g active substance/ha is calculated from information
#' on dosage (product concentration in the application solution) and application volume,
#' or directly from the product application rate. This is complicated by the fact
#' that a rate ("expenditure" in the XML file) with units l/ha can refer
#' to the application solution or to the liquid product.
#'
#' In some cases (currently one), external information was found, indicating
#' that the "expenditure" is an application volume [l_per_ha_is_water_volume].
#'
#' @note A reference application volume is used if there is no 'expenditure'.
#' It is selected only based on the product application area. This is not correct
#' if hops ('Hopfen') is the culture, as it has a unique reference application
#' volume of 3000 L/ha.
#'
#' Applications to hops were excluded for calculating mean use rates in the
#' indicator project (Korkaric 2023), arguing that it is not grown in large
#' areas in Switzerland.
#' @param product_uses A tibble containing the columns 'pNbr', 'use_nr',
#' 'application_area_de', 'min_dosage', 'max_dosage', 'min_rate', 'max_rate',
#' from the 'uses' table in a [srppp_dm] object, as well as the columns
#' 'percent' and 'g_per_L' from the 'ingredients' table in a [srppp_dm] object.
#' @param aggregation How to represent a range if present, e.g. "max" (default)
#' or "mean".
#' @param dosage_units If no units are given, or units are "%", then the applied
#' amount in g/ha is calculated using a reference application volume and the
#' dosage. As the dosage units are not explicitly given, we can specify our
#' assumptions about these using this argument (currently not implemented, i.e.
#' specifying the argument has no effect).
#' @param skip_l_per_ha_without_g_per_L Per default, uses where the use rate
#' has units of l/ha are skipped, if there is not product concentration
#' in g/L. This was also done in the 2023 indicator project.
#' @param fix_l_per_ha During the review of the 2023 indicator
#' project calculations, a number of cases were identified where the unit
#' l/ha specifies a water volume, and not a product volume. If TRUE (default),
#' these cases are corrected, if FALSE, these cases are discarded.
#' @return A tibble containing one additional column 'rate_g_per_ha'
#' @export
#' @examples
#' \donttest{
#' library(srppp)
#' library(dplyr, warn.conflicts = FALSE)
#' library(dm, warn.conflicts = FALSE)
#'
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
#' product_uses_with_ingredients <- sr$substances |>
#'   filter(substance_de %in% c("Halauxifen-methyl", "Kupfer (als Kalkpr\u00E4parat)")) |>
#'   left_join(sr$ingredients, by = "pk") |>
#'   left_join(sr$uses, by = "pNbr") |>
#'   left_join(sr$products, by = "pNbr") |>
#'   select(pNbr, name, use_nr,
#'     min_dosage, max_dosage, min_rate, max_rate, units_de,
#'     application_area_de,
#'     substance_de, percent, g_per_L)
#'
#' application_rate_g_per_ha(product_uses_with_ingredients) |>
#'   filter(name %in% c("Cerelex", "Pixxaro EC", "Bordeaux S")) |>
#'   select(ai = substance_de, app_area = application_area_de,
#'     min_d = min_dosage,  max_d = max_dosage,
#'     min_r = min_rate, max_r = max_rate,
#'     units_de, rate = rate_g_per_ha) |>
#'   print(n = Inf)
#' }
application_rate_g_per_ha <- function(product_uses,
  aggregation = c("max", "mean", "min"),
  dosage_units = c("percent_ww", "percent_vv", "state_of_matter"),
  skip_l_per_ha_without_g_per_L = TRUE,
  fix_l_per_ha = TRUE)
{
  aggregation = match.arg(aggregation)
  rates_dosages <- product_uses |> # Rates are called "Expenditures" in the XML
    mutate( # First we set zeros to NA, as this is what they are
      min_rate = na_if(min_rate, 0),
      max_rate = na_if(max_rate, 0),
      min_dosage = na_if(min_dosage, 0),
      max_dosage = na_if(max_dosage, 0)) |>
    mutate( # Then we make ranges, possibly with min equals max
      rate_min = min_rate, # 'expenditures' are use rates (Aufwandmengen)
      rate_max = if_else(is.na(max_rate), # if zero, either no range or not given
        min_rate, max_rate),
      dosage_min = min_dosage, # 'dosages' are in-use concentrations in percent
      dosage_max = if_else(is.na(max_dosage),
        min_dosage, max_dosage)
      ) |>
    mutate(
      rate = case_when(
        aggregation == "mean" ~ (rate_min + rate_max)/2,
        aggregation == "max" ~ rate_max,
        aggregation == "min" ~ rate_min
      ),
      dosage = case_when(
        aggregation == "mean" ~ (dosage_min + dosage_max)/2,
        aggregation == "max" ~ dosage_max,
        aggregation == "min" ~ dosage_min
      )
    )

  active_rates <- rates_dosages |>
    mutate(ref_volume = case_when(
      application_area_de %in% c("Feldbau", "Gem\u00FCsebau", "Beerenbau", "Zierpflanzen") ~ 1000,
      application_area_de %in% c("Weinbau", "Obstbau") ~ 1600,
      .default = NA)) |>
    left_join(l_per_ha_is_water_volume, by = c("pNbr", "use_nr")) |>
    mutate(rate_g_per_ha = case_when(
      units_de == "l/ha" ~ # l/ha can refer to product or water volume
        if_else(is.na(source), # if no external information, assume l/ha is product
          if_else(is.na(g_per_L), # if g_per_L is not defined
            if (skip_l_per_ha_without_g_per_L) NA # as in the 2023 indicator
            else {
              if_else(is.na(dosage),
                # If we have no dosage, treat l/ha as kg/ha and use percent to calculate rate_g_per_ha.
                rate * (percent/100) * 1000, # Correct for Metro 2017
                # If we have a dosage, the rate in l/ha is assumed to be the application solution
                rate * dosage * (percent/100)) # Correct for Rhodofix 2009 (Grünbuch) and 2012 (XML)
            },
            rate * g_per_L), # l/ha is product
          if (fix_l_per_ha) { # Sometimes we have external information that the rate in l/ha is water
            rate * dosage/100 * g_per_L
          } else NA),
      units_de == "kg/ha" ~ rate * (percent * 10), # percent w/w means 10 g/kg
      units_de == "g/ha" ~ rate * (percent / 100), # percent w/w means 0.01 g/g
      units_de == "ml/m\u00B2" ~ (rate/1000) * (g_per_L) * 10000,
      units_de == "ml/10m\u00B2" ~ (rate/1000) * (g_per_L) * 1000,
      units_de == "ml/ha" ~ (rate/1000) * (g_per_L),
      units_de == "ml/a" ~ (rate/1000) * (g_per_L) * 100,
      is.na(units_de) & !is.na(g_per_L) ~ # g_per_L available -> liquid
        ref_volume * dosage/100 * g_per_L, # dosage assumed to be v/v
      is.na(units_de) & is.na(g_per_L) ~ # only percent available -> solid
        ref_volume * 1000 * # 1 L spraying solution equivalent to 1000 g
        dosage/100 * percent/100, # dosage assumed to be w/w
      .default = NA))
  ret <- bind_cols(product_uses, active_rates["rate_g_per_ha"])
  return(ret)
}

#' Product application rate units convertible to grams active substance per hectare
#'
#' @docType data
#' @export
#' @seealso [application_rate_g_per_ha]
#' @examples
#' library(srppp)
#' library(dplyr)
#' # These are the convertible units
#' units_convertible_to_g_per_ha
units_convertible_to_g_per_ha <- c("l/ha", "kg/ha", "g/ha",
  "ml/m\u00B2", "ml/10m\u00B2", "ml/ha", "ml/a")

#' Use definitions where the rate in l/ha refers to the volume of the spraying solution
#'
#' @docType data
#' @export
#' @seealso [application_rate_g_per_ha]
#' @examples
#' library(srppp)
#' l_per_ha_is_water_volume
l_per_ha_is_water_volume <- tibble::tribble(
  ~ pNbr, ~ use_nr, ~ source, ~ url,
  5151L, 1L, "EFSA conclusion on cyanamide 2010, p. 17",
  "https://doi.org/10.2903/j.efsa.2010.1873"
)
