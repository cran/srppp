## ----setup, include = TRUE, message = FALSE-----------------------------------
knitr::opts_chunk$set(tidy = FALSE, cache = FALSE)
options(knitr.kable.NA = '')
library(srppp)
library(dplyr)
library(knitr)

## ----srppp--------------------------------------------------------------------
srppp <- try(srppp_dm(srppp_xml_url))

# Fall back to using the file distributed with the package
if (inherits(srppp, "try-error")) {
  test_data <- system.file("testdata/Daten_Pflanzenschutzmittelverzeichnis_2024-12-16.zip",
  package = "srppp")
  test_xml <- srppp_xml_get_from_path(test_data, from = "2024-12-16")
  srppp <- srppp_dm(test_xml)
}

## ----warning = FALSE----------------------------------------------------------
srppp$categories |>
  group_by(category_de) |>
  summarise(n = n()) |>
  kable()

## -----------------------------------------------------------------------------
microorganism_categories = c(
  "Lebende Organismen (Bakterien)",
  "Lebende Organismen (Pilze)",
  "Lebende Organismen (gegen Pilze)")
kable(data.frame(Category = microorganism_categories))

## -----------------------------------------------------------------------------
products_in_microorganism_categories <- srppp$products |>
  filter(!isSalePermission) |>
  left_join(srppp$categories, by = "pNbr") |>
  filter(category_de %in% microorganism_categories) |>
  select(pNbr, wNbr, name, category_de) |>
  arrange(pNbr)
n_in_categories <- nrow(products_in_microorganism_categories)
kable(products_in_microorganism_categories)

## -----------------------------------------------------------------------------
srppp$products |>
  filter(!isSalePermission) |>
  left_join(srppp$categories, by = "pNbr") |>
  filter(category_de == "Insektizid") |>
  left_join(srppp$ingredients, by = "pNbr", relationship = "many-to-many") |>
  left_join(srppp$substances, by = "pk") |>
  select(substance_de) |>
  unique() |>
  arrange(substance_de) |>
  kable()

srppp$products |>
  filter(!isSalePermission) |>
  left_join(srppp$categories, by = "pNbr") |>
  filter(category_de == "Fungizid") |>
  left_join(srppp$ingredients, by = "pNbr", relationship = "many-to-many") |>
  left_join(srppp$substances, by = "pk") |>
  select(substance_de) |>
  unique() |>
  arrange(substance_de) |>
  kable()

## -----------------------------------------------------------------------------
microorganism_genus_names <- c(
  "Bacillus",
  "Beauveria",
  "Metarhizium",
  "Xenorhabdus",
  "Phlebia",
  "Pseudomonas")
kable(data.frame("Genus names" = microorganism_genus_names, check.names = FALSE))

## -----------------------------------------------------------------------------
microorganism_regexp <- paste(microorganism_genus_names, collapse = "|")
microorganism_ingredients <- srppp$substances |>
  filter(grepl(microorganism_regexp, substance_de)) |>
  select(pk, substance_de)
kable(microorganism_ingredients)


## -----------------------------------------------------------------------------
additional_products_containing_microorganisms <- srppp$products |>
  filter(!isSalePermission) |>
  left_join(srppp$ingredients, by = "pNbr", relationship = "many-to-many") |>
  filter(pk %in% microorganism_ingredients$pk) |>
  select(-pk) |>
  left_join(srppp$categories, by = "pNbr") |> 
  select(pNbr, wNbr, name, category_de) |>
  arrange(pNbr)
n_additional <- nrow(additional_products_containing_microorganisms)
kable(additional_products_containing_microorganisms)

## -----------------------------------------------------------------------------
products_containing_microorganisms <- rbind(
  products_in_microorganism_categories, 
  additional_products_containing_microorganisms) |>
  arrange(pNbr, category_de) |>
  unique()
n_unique <- length(unique(products_containing_microorganisms$pNbr))

## -----------------------------------------------------------------------------
kable(products_containing_microorganisms)

