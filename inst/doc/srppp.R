## ----setup, message = FALSE, echo = FALSE-------------------------------------
library(knitr)
knitr::opts_chunk$set(tidy = FALSE, cache = FALSE)
options(knitr.kable.NA = '')

## ----message = FALSE----------------------------------------------------------
library(srppp)
library(dplyr)
example_register <- try(srppp_dm())

## -----------------------------------------------------------------------------
if (inherits(example_register, "try-error")) {
  test_data <- system.file("testdata/Daten_Pflanzenschutzmittelverzeichnis_2024-12-16.zip",
  package = "srppp")
  test_xml <- srppp_xml_get_from_path(test_data, from = "2024-12-16")
  example_register <- srppp_dm(test_xml)
}

## -----------------------------------------------------------------------------
library(DiagrammeR)
dm_draw(example_register)

## -----------------------------------------------------------------------------
library(knitr)
example_register$substances |> 
  select(pk, iupac, substance_de, substance_fr, substance_it) |> 
  head(n = 4L) |> 
  kable()

## -----------------------------------------------------------------------------
example_register$ingredients |> 
  select(pNbr, pk, type, percent, g_per_L, ingredient_de, ingredient_fr) |> 
  head(n = 5L) |> 
  kable()

## ----message = FALSE----------------------------------------------------------
library(dplyr)
example_register$ingredients |> 
  select(pk, type) |> 
  unique() |> 
  group_by(type) |> 
  summarize(n = n()) |> 
  kable()

## -----------------------------------------------------------------------------
example_register$ingredients |> 
  left_join(example_register$substances, by = "pk") |>
  filter(type %in% c("SYNERGIST", "SAFENER")) |>
  group_by(type, substance_de) |> 
  summarize(n = n(), .groups = "drop_last") |> 
  select(type, substance_de, n) |> 
  arrange(type, substance_de) |> 
  kable()

## ----eval = FALSE-------------------------------------------------------------
# example_register |>
#   dm_flatten_to_tbl(ingredients) |>

## -----------------------------------------------------------------------------
example_register$products |> 
  select(-terminationReason) |> 
  head() |> 
  kable()

## -----------------------------------------------------------------------------
example_register$products |>
  filter(exhaustionDeadline != "") |> 
  select(-terminationReason) |> 
  head() |> 
  kable()

## ----echo = FALSE-------------------------------------------------------------
n_pNbrs <- nrow(example_register$pNbrs)
n_wNbrs <- nrow(example_register$products)

## -----------------------------------------------------------------------------
example_register$products |>
  filter(name == "Plüsstar") |>
  left_join(example_register$ingredients, by = "pNbr") |>
  left_join(example_register$substances, by = "pk") |>
  select(pNbr, name, substance_de, percent, g_per_L) |> 
  kable()

## -----------------------------------------------------------------------------
example_register$uses |> 
  filter(pNbr %in% c(6521L, 7511L) & use_nr < 10) |> 
  select(pNbr, use_nr, ends_with("dosage"), ends_with("rate"), units_de,
    waiting_period, time_units_en, application_area_de) |> 
  head(20) |> 
  kable()

## -----------------------------------------------------------------------------
example_uses <- example_register$products |> 
  filter(wNbr == "6168") |>
  left_join(example_register$uses, by = join_by(pNbr),
    relationship = "many-to-many") |> 
  left_join(example_register$ingredients, by = join_by(pNbr),
    relationship = "many-to-many") |>
  left_join(example_register$substances, by = join_by(pk)) |>
  select(pNbr, name, use_nr,
    min_dosage, max_dosage, min_rate, max_rate, units_de,
    application_area_de,
    substance_de, percent, g_per_L) |> 
  filter(use_nr %in% c(1:5, 12:17))

kable(example_uses)

## -----------------------------------------------------------------------------
application_rate_g_per_ha(example_uses) |>
  select(ai = substance_de, app_area = application_area_de,
  ends_with("rate"), units_de, rate = rate_g_per_ha) |> 
  head(n = 14) |> 
  kable()

## -----------------------------------------------------------------------------
example_register$culture_forms |> 
  select(starts_with("culture")) |> 
  unique() |> 
  kable()

## -----------------------------------------------------------------------------
example_register$products |> 
  filter(wNbr == "4458") |> 
  left_join(example_register$uses, by = "pNbr") |> 
  filter(use_nr %in% c(1, 10)) |> 
  left_join(example_register$culture_forms, by = c("pNbr", "use_nr")) |> 
  left_join(example_register$cultures, by = c("pNbr", "use_nr")) |> 
  select(pNbr, use_nr, application_area_de, culture_form_de, culture_de) |> 
  kable()

## -----------------------------------------------------------------------------
culture_tree <- attr(example_register, "culture_tree")
print(culture_tree, limit = 30, "culture_id")

## -----------------------------------------------------------------------------
example_register$pests |> 
  filter(pNbr == 7105L, use_nr %in% 1:2) |> 
  select(use_nr, ends_with("de"), ends_with("fr")) |> 
  kable()

## -----------------------------------------------------------------------------
culture_pest_combinations <- example_register$uses |> 
  filter(pNbr == 6521L) |> 
  left_join(example_register$cultures, by = c("pNbr", "use_nr")) |> 
  left_join(example_register$pests, by = c("pNbr", "use_nr")) |> 
  select(pNbr, use_nr, application_area_de, culture_de, pest_de)

kable(culture_pest_combinations)

## -----------------------------------------------------------------------------
example_register$application_comments |>
  filter(pNbr == 7105, use_nr %in% 1:2) |> 
  select(pNbr, use_nr, ends_with("de"), ends_with("fr")) |> 
  kable()

## -----------------------------------------------------------------------------
example_register$obligations |>
  filter(pNbr == 7105, use_nr %in% 1:2) |> 
  select(pNbr, use_nr, code, obligation_de, sw_runoff_points) |> 
  kable()

## -----------------------------------------------------------------------------
print(culture_tree, "culture_id", "name_fr", "name_it", limit = 800)

