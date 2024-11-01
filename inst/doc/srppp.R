## ----setup, message = FALSE, echo = FALSE-------------------------------------
library(knitr)
knitr::opts_chunk$set(tidy = FALSE, cache = FALSE)
options(knitr.kable.NA = '')

## ----message = FALSE----------------------------------------------------------
library(srppp)
current_register <- srppp_dm()

## -----------------------------------------------------------------------------
library(DiagrammeR)
dm_draw(current_register)

## -----------------------------------------------------------------------------
library(knitr)
current_register$substances |> 
  select(pk, iupac, substance_de, substance_fr, substance_it) |> 
  head(n = 4L) |> 
  kable()

## -----------------------------------------------------------------------------
current_register$ingredients |> 
  select(pNbr, pk, type, percent, g_per_L, ingredient_de, ingredient_fr) |> 
  head(n = 5L) |> 
  kable()

## ----message = FALSE----------------------------------------------------------
library(dplyr)
current_register$ingredients |> 
  select(pk, type) |> 
  unique() |> 
  group_by(type) |> 
  summarize(n = n()) |> 
  kable()

## -----------------------------------------------------------------------------
current_register$ingredients |> 
  left_join(current_register$substances, by = "pk") |>
  filter(type %in% c("SYNERGIST", "SAFENER")) |>
  group_by(type, substance_de) |> 
  summarize(n = n(), .groups = "drop_last") |> 
  select(type, substance_de, n) |> 
  arrange(type, substance_de) |> 
  kable()

## ----eval = FALSE-------------------------------------------------------------
#  current_register |>
#    dm_flatten_to_tbl(ingredients) |>

## -----------------------------------------------------------------------------
current_register$products |> 
  select(-terminationReason) |> 
  head() |> 
  kable()

## -----------------------------------------------------------------------------
current_register$products |>
  filter(exhaustionDeadline != "") |> 
  select(-terminationReason) |> 
  head() |> 
  kable()

## ----echo = FALSE-------------------------------------------------------------
n_pNbrs <- nrow(current_register$pNbrs)
n_wNbrs <- nrow(current_register$products)

## -----------------------------------------------------------------------------
current_register$products |>
  filter(name == "Plüsstar") |>
  left_join(current_register$ingredients, by = "pNbr") |>
  left_join(current_register$substances, by = "pk") |>
  select(pNbr, name, substance_de, percent, g_per_L) |> 
  kable()

## -----------------------------------------------------------------------------
current_register$uses |> 
  filter(pNbr %in% c(6521L, 7511L) & use_nr < 10) |> 
  select(pNbr, use_nr, ends_with("dosage"), ends_with("rate"), units_de,
    waiting_period, time_units_en, application_area_de) |> 
  head(20) |> 
  kable()

## -----------------------------------------------------------------------------
example_uses <- current_register$products |> 
  filter(wNbr == "6168") |>
  left_join(current_register$uses, by = join_by(pNbr),
    relationship = "many-to-many") |> 
  left_join(current_register$ingredients, by = join_by(pNbr),
    relationship = "many-to-many") |>
  left_join(current_register$substances, by = join_by(pk)) |>
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
current_register$culture_forms |> 
  select(starts_with("culture")) |> 
  unique() |> 
  kable()

## -----------------------------------------------------------------------------
current_register$products |> 
  filter(wNbr == "6168") |> 
  left_join(current_register$uses, by = "pNbr") |> 
  filter(use_nr %in% 1:2) |> 
  left_join(current_register$culture_forms, by = c("pNbr", "use_nr")) |> 
  select(pNbr, use_nr, ends_with("de")) |> 
  kable()

## -----------------------------------------------------------------------------
current_register$pests |> 
  filter(pNbr == 7105L, use_nr %in% 1:2) |> 
  select(use_nr, ends_with("de"), ends_with("fr")) |> 
  kable()

## -----------------------------------------------------------------------------
culture_pest_combinations <- current_register$uses |> 
  filter(pNbr == 6521L) |> 
  left_join(current_register$cultures, by = c("pNbr", "use_nr")) |> 
  left_join(current_register$pests, by = c("pNbr", "use_nr")) |> 
  select(pNbr, use_nr, application_area_de, culture_de, pest_de)

kable(culture_pest_combinations)

## -----------------------------------------------------------------------------
current_register$application_comments |>
  filter(pNbr == 7105, use_nr %in% 1:2) |> 
  select(pNbr, use_nr, ends_with("de"), ends_with("fr")) |> 
  kable()

## -----------------------------------------------------------------------------
current_register$obligations |>
  filter(pNbr == 7105, use_nr %in% 1:2) |> 
  select(pNbr, use_nr, code, obligation_de, sw_runoff_points) |> 
  kable()

