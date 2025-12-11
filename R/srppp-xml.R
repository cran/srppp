utils::globalVariables(c("id", "name", "pk", "wNbr", "wGrp", "pNbr", "use_nr",
  "add_txt_pk", "de", "fr", "it", "en", "exhaustionDeadline", "soldoutDeadline",
  "isSalePermission", "terminationReason", "substance_de",
  "packageInsert", "permission_holder", "producingCountryPrimaryKey",
  "ingredient_de", "ingredient_fr", "ingredient_it", "categories",
  "min_dosage", "max_dosage", "min_rate", "max_rate", "waiting_period",
  "biotope_drift_dist", "sw_drift_dist", "sw_runoff_dist", "sw_runoff_points",
  "desc_pk", "ingr_desc_pk",
  "units_pk", "time_units_pk",
  "type", "g_per_L", "percent"))

#' Read an XML version of the Swiss Register of Plant Protection Products
#'
#' @param from A specification of the way to retrieve the XML
#' @param \dots Unused argument introduced to facilitate future extensions
#' @return An object inheriting from 'srppp_xml', 'xml_document', 'xml_node'
#' @export
srppp_xml_get <- function(from, ...)
{
  UseMethod("srppp_xml_get")
}

#' @rdname srppp_xml_get
#' @export
#' @examples
#' # Try to get the current SRPPP as available from the FOAG website
#' \donttest{
#' srppp_xml_cur <- try(srppp_xml_get())
#' }
srppp_xml_get.NULL <- function(from, ...)
{
  from <- srppp_xml_url
  path <- tempfile(fileext = "zip")
  download.file(from, path)

  srppp_xml_get_from_path(path, from)
}

#' @rdname srppp_xml_get
#' @export
srppp_xml_get.character <- function(from, ...)
{
  path <- tempfile(fileext = "zip")
  download.file(from, path)

  srppp_xml_get_from_path(path, from)
}

#' @rdname srppp_xml_get
#' @param path A path to a zipped SRPPP XML file
#' @export
srppp_xml_get_from_path <- function(path, from) {
  if (grepl("zip$", path)) {
    zip_contents <- utils::unzip(path, list = TRUE)
    xml_filename <- grep("PublicationData_20.._.._...xml",
      zip_contents$Name, value = TRUE)
    xml_con <- unz(path, xml_filename)
    ret <- read_xml(xml_con)
  } else {
    if (grepl("xz$", path)) {
      xml_con <- xzfile(path)
      ret <- read_xml(xml_con)
    }
  }

  # Determine the version of the XML file and attach it to the returned object
  substance_type_node <- xml_find_first(ret,
    "Products/Product/ProductInformation/Ingredient/SubstanceType")

  if (is.na(xml_attr(substance_type_node, "SubstanceType"))) {
    attr(ret, "srppp_xml_version") <- 1
  } else {
    attr(ret, "srppp_xml_version") <- 2
  }

  # Attach the "from" information as well
  attr(ret, "from") <- as.character(from)

  class(ret) <- c("srppp_xml", "xml_document", "xml_node")
  return(ret)
}

#' Get Products from an XML version of the Swiss Register of Plant Protection Products
#'
#' @param srppp_xml An object as returned by 'srppp_xml_get'
#' @param verbose Should we give some feedback?
#' @param remove_duplicates Should duplicates based on wNbrs be removed? If set
#' to 'TRUE', one of the two entries with identical wNbrs is removed, based on
#' an investigation of background information carried out by the package authors.
#' In all cases except for one, one of the product sections with duplicate wNbrs
#' has information about an expiry of the registration, and the other doesn't.
#' In these cases the registration without expiry is kept, and the expiring
#' registration is discarded. In the remaining case (wNbr 5945), the second
#' entry is selected, as it contains more indications which were apparently
#' intended to be published as well.
#' @return A [tibble::tibble] with a row for each product section
#' in the XML file. An attribute 'duplicated_wNbrs' is
#' also returned, containing duplicated W-Numbers, if applicable,
#' or NULL.
#' @export
#' @examples
#' # Try to get current list of products
#' \donttest{
#' try(srppp_xml_get_products())
#' }
srppp_xml_get_products <- function(srppp_xml = srppp_xml_get(), verbose = TRUE,
  remove_duplicates = TRUE)
{
  product_nodeset <- xml_find_all(srppp_xml, "Products/Product")
  product_attribute_names <- names(xml_attrs(product_nodeset[[1]]))

  product_pNbrs <- product_nodeset |>
    xml_attrs() |>
    unlist() |>
    matrix(ncol = 7, byrow = TRUE,
      dimnames = list(NULL, product_attribute_names)) |>
    as_tibble() |>
    filter(!grepl("-", wNbr)) |> # remove sales permissions
    mutate(pNbr = as.integer(id)) |>
    mutate(wGrp = wNbr) |> # we define a "W-Number group", i.e. the part of the W-Number before the dash
    select(wGrp, pNbr)

  products_no_permission_holders <- product_nodeset |>
    xml_attrs() |>
    unlist() |>
    matrix(ncol = 7, byrow = TRUE,
      dimnames = list(NULL, product_attribute_names)) |>
    as_tibble() |>
    mutate(wGrp = gsub("-.*$", "", wNbr)) |>
    mutate(isSalePermission = if_else(isSalePermission == "true", TRUE, FALSE)) |>
    left_join(product_pNbrs, by = "wGrp") |>
    select(wNbr, name, pNbr, exhaustionDeadline, soldoutDeadline,
      isSalePermission, terminationReason)

  ph_nodes <- xml_find_all(srppp_xml,
    "Products/Product/ProductInformation/PermissionHolderKey")

  ph_key_matrix <- t(sapply(ph_nodes, function(node) {
    wNbr <- xml_attr(xml_parent(xml_parent(node)), "wNbr")
    ph_key <- xml_attr(node, "primaryKey")
    c(wNbr, ph_key)
  }))
  colnames(ph_key_matrix) <- c("wNbr", "permission_holder")
  ph_keys <- as_tibble(ph_key_matrix)
  ph_keys$permission_holder <- ph_keys$permission_holder

  # Discard the second permission holder
  # The code was developed for ParallelImports, where this occurred
  ph_keys <- ph_keys[!duplicated(ph_keys$wNbr), ]

  products <- products_no_permission_holders |>
    left_join(ph_keys, by = "wNbr")

  # Check if identical P-Numbers have identical
  # product sections except for PermissionHolderKey
  for (pNbr in sort(unique(products$pNbr))) {

    # Get node indices with the current P-Number
    node_numbers <- which(pNbr == products$pNbr)

    # Compare them if there is more than one
    if (length(node_numbers) > 1) {

      # Define the reference node (original product)
      ref_node <- xml_child(product_nodeset[[node_numbers[1]]])
      ref_contents <- unlist(strsplit(as.character(ref_node), "\n"))

      # In certain cases (2025-12-01, W-Number 7255), there is no permission holder
      if (any(grepl("PermissionHolderKey", ref_contents))) {
        ref_contents_permholder_i <- grep("PermissionHolderKey", ref_contents)
        ref_contents_clean <- ref_contents[-ref_contents_permholder_i]
      } else {
        ref_contents_clean <- ref_contents
      }

      # Loop over products
      for (i in 2:length(node_numbers)) {
        check_node <- xml_child(product_nodeset[[node_numbers[i]]])
        check_contents <- unlist(strsplit(as.character(check_node), "\n"))

        # In certain cases (2011-10-31, P-Number 7539), there is no
        # PermissionHolderKey for a sales permission
        if (any(grepl("PermissionHolderKey", check_contents))) {
          check_contents_permholder_i <- grep("PermissionHolderKey", check_contents)
          check_contents_clean <- check_contents[-check_contents_permholder_i]
        } else {
          check_contents_clean <- check_contents
        }

        if (!identical(ref_contents_clean, check_contents_clean)) {
          cli::cli_alert_warning(
            paste0("P-Number ", pNbr, ": Differing product section for W-Number ",
              products$wNbr[node_numbers[[i]]])
          )
          if (verbose) {
            message(waldo::compare(ref_contents_clean, check_contents_clean))
          }
        }
      }
    }
  }
  dup_index <- which(duplicated(products$wNbr))
  dup_wNbrs <- products[dup_index, ]$wNbr

  if (remove_duplicates) {

    # See documentation of argument 'remove_duplicates'
    known_duplicates_expired_and_renewed <- as.character(
      c(
        6721, # Cueva
        5807, # Maxim XL
        6241, # Heritage
        6274, # Ranman
        5463, # Monitor
        2743, # Vulkan- Wühlmauspatrone
        4343, # Cypermethrin
        4009  # Lentagran WP
      )
    )
    known_duplicates_take_second <- "5945"

    i_products_to_remove <- NULL
    for (dup_wNbr in dup_wNbrs) {
      if (dup_wNbr %in% known_duplicates_expired_and_renewed) {
        which(products$wNbr == dup_wNbr)
        length(products$exhaustionDeadline != "")
        length(products$wNbr == dup_wNbr)

        i_expired <- which(products$wNbr == dup_wNbr & products$exhaustionDeadline != "")
        if (verbose) {
          cli::cli_alert_warning(
            paste("Removing entry with expiration date for duplicated W-Number:", dup_wNbr))
        }
        i_products_to_remove <- c(i_products_to_remove, i_expired)
      } else {
        if (dup_wNbr %in% known_duplicates_take_second) {
          i_second <- which(products$wNbr == dup_wNbr)[2]
          if (verbose) {
            cli::cli_alert_warning(
              paste("Removing second entry for duplicated W-Number:", dup_wNbr))
          }
          i_products_to_remove <- c(i_products_to_remove, i_second)
        } else {
          stop("Unknown duplicated W-Number:", dup_wNbr)
        }
      }
    }
    if (!is.null(i_products_to_remove)) {
      products <- products[-i_products_to_remove, ]
    }
    attr(products, "duplicated_wNbrs") = NULL
  } else {
    attr(products, "duplicated_wNbrs") <- dup_wNbrs
  }
  products_to_return <- products |>
    select(pNbr, wNbr, name,
      exhaustionDeadline, soldoutDeadline,
      isSalePermission, terminationReason,
      permission_holder) |>
    arrange(pNbr, wNbr)

  return(products_to_return)
}

#' Get Parallel Imports from an XML version of the Swiss Register of Plant Protection Products
#'
#' @inheritParams srppp_xml_get_products
#' @return A [tibble::tibble] with a row for each parallel import section
#' in the XML file.
#' @export
#' @examples
#' # Try to get current list of parallel_imports
#' \donttest{
#' try(srppp_xml_get_parallel_imports())
#' }
srppp_xml_get_parallel_imports <- function(srppp_xml = srppp_xml_get())
{
  pi_nodeset <- xml_find_all(srppp_xml, "Parallelimports/Parallelimport")
  pi_attribute_names <- names(xml_attrs(pi_nodeset[[1]]))
  pis <- pi_nodeset |>
    xml_attrs() |>
    unlist() |>
    matrix(ncol = 8, byrow = TRUE,
      dimnames = list(NULL, pi_attribute_names)) |>
    as_tibble() |>
    rename(pNbr = packageInsert) |>
    mutate(pNbr = as.integer(pNbr)) |>
    arrange(wNbr)

  ph_nodes <- xml_find_all(srppp_xml,
    "Parallelimports/Parallelimport/ProductInformation/PermissionHolderKey")

  ph_key_matrix <- t(sapply(ph_nodes, function(node) {
    pi_id <- xml_attr(xml_parent(xml_parent(node)), "id")
    ph_key <- xml_attr(node, "primaryKey")
    c(pi_id, ph_key)
  }))
  colnames(ph_key_matrix) <- c("id", "permission_holder")
  ph_keys <- as_tibble(ph_key_matrix)

  # Discard the second permission holder
  # For example, in the XML file from 2019-03-05, the Parallelimport F-6146
  # has two PermissionHolderKey sections, with different primaryKey attributes
  ph_keys <- ph_keys[!duplicated(ph_keys$id), ]

  retval <- pis |>
    left_join(ph_keys, by = "id")

  return(retval)
}

#' Get substances from an XML version of the Swiss Register of Plant Protection Products
#'
#' @param srppp_xml An object as returned by 'srppp_xml_get'
#' @return A [tibble::tibble] containing primary keys, IUPAC names and substance names in
#' German, French and Italian.
#' @export
#' @examples
#' \donttest{
#' try(srppp_xml_get_substances())
#' }
srppp_xml_get_substances <- function(srppp_xml = srppp_xml_get()) {
  substance_nodeset <- xml_find_all(srppp_xml, "MetaData[@name='Substance']/Detail")

  sub_desc <- t(sapply(substance_nodeset, function(sub_node) {
    desc_nodes <- xml_children(sub_node)

    substance_info <- trimws(c(
      xml_attr(sub_node, "primaryKey"),
      xml_attr(sub_node, "iupacName"),
      xml_attr(desc_nodes, "value")))

    xml_attr(desc_nodes, "language")

    names(substance_info) <- c("pk", "iupac", paste0("substance_", xml_attr(desc_nodes, "language")))
    return(substance_info[c("pk", "iupac", paste0("substance_", c("de", "fr", "it", "en")))])
  }))

  ret <- as_tibble(sub_desc) |>
    dplyr::arrange(substance_de)

  return(ret)
}

#' Get ingredients for all registered products described in an XML version of the Swiss Register of Plant Protection Products
#'
#' @param srppp_xml An object as returned by 'srppp_xml_get'
#' @return A [tibble::tibble] containing a line for each ingredient of each W-Number
#' @export
#' @examples
#' \donttest{
#' try(srppp_xml_get_ingredients())
#' }
srppp_xml_get_ingredients <- function(srppp_xml = srppp_xml_get())
{
  ingredient_nodeset <- xml_find_all(srppp_xml,
    "Products/Product/ProductInformation/Ingredient")

  get_ingredient_map <- function(ingredient_node) {
    wNbr <- xml_attr(xml_parent(xml_parent(ingredient_node)), "wNbr")
    pk <- toupper(xml_attr(xml_child(ingredient_node, search = 2), "primaryKey")) # In some cases we have lower case UUIDs here
    type <- xml_text(xml_child(ingredient_node, search = 1))
    attributes <- xml_attrs(ingredient_node)
    ret <- c(wNbr, pk, type,
      attributes[c("inPercent", "inGrammPerLitre", "additionalTextPrimaryKey")])
    names(ret) <- c("wNbr", "pk", "type", "percent", "g_per_L", "add_txt_pk")

    return(ret)
  }

  # As the contents of additives are confidential, we remove them to address cases
  # were they were accidentally included in the XML dump.
  ingredients <- t(sapply(ingredient_nodeset, get_ingredient_map)) |>
    as_tibble() |>
    mutate(percent = if_else(
      type == "ADDITIVE_TO_DECLARE", "", percent)) |>
    mutate(g_per_L = if_else(
      type == "ADDITIVE_TO_DECLARE", "", g_per_L))

  ingredient_descriptions <- srppp_xml |>
    xml_find_all(paste0("MetaData[@name='IngredientAdditionalText']/Detail")) |>
    sapply(get_descriptions, code = FALSE) |> t() |>
    as_tibble() |>
    rename(ingredient_de = de, ingredient_fr = fr) |>
    rename(ingredient_it = it, ingredient_en = en) |>
    rename(ingr_desc_pk = desc_pk) |>
    arrange(ingr_desc_pk)

  ret <- ingredients |>
    filter(!grepl("-", wNbr)) |>
    left_join(ingredient_descriptions, by = c(add_txt_pk = "ingr_desc_pk")) |>
    select(-add_txt_pk) |>
    mutate(across(c(percent, g_per_L), as.numeric))

  ret_corrected <- ret |>
  # Active substance content of Dormex (W-3066) is not 667 g/L, but 520 g/L
  # Wädenswil archive, Johannes Ranke and Daniel Baumgartner, 2024-03-27:
    mutate(
      percent = if_else(wNbr == "3066", 49, percent),
      g_per_L = if_else(wNbr == "3066", 520, g_per_L)) |>
    arrange(wNbr, pk, type)

  return(ret_corrected)
}

#' Define use identification numbers in an SRPPP read in from an XML file
#'
#' @param srppp_xml An object as returned by [srppp_xml_get]
#' @return An object of the same class, with 'use_nr' added as an attribute of 'Indication' nodes.
#' @export
#' @examples
#' \donttest{
#' try(srppp_xml_define_use_numbers())
#' }
srppp_xml_define_use_numbers <- function(srppp_xml = srppp_xml_get()) {
  use_nodeset <- xml_find_all(srppp_xml, "Products/Product/ProductInformation/Indication")

  uses <- tibble(wNbr = sapply(use_nodeset, get_grandparent_wNbr)) |>
    group_by(wNbr) |>
    mutate(use_nr = sequence(rle(wNbr)$length)) # https://stackoverflow.com/a/46613159

  xml_attr(use_nodeset, "use_nr") <- uses$use_nr

  return(srppp_xml)
}

#' Get uses ('indications') for all products described in an XML version of the Swiss Register of Plant Protection Products
#'
#' @param srppp_xml An object as returned by [srppp_xml_get] with use numbers
#' defined by [srppp_xml_define_use_numbers]
#' @return A [tibble::tibble] of use definitions
#' @export
#' @examples
#' \donttest{
#' srppp_xml <- try(srppp_xml_get())
#' if (!inherits(srppp_xml, "try-error")) {
#'   srppp_xml <- srppp_xml_define_use_numbers(srppp_xml)
#'   srppp_xml_get_uses(srppp_xml)
#' }
#' }
srppp_xml_get_uses <- function(srppp_xml = srppp_xml_get()) {
  use_nodeset <- xml_find_all(srppp_xml, "Products/Product[not(contains(@wNbr, '-'))]/ProductInformation/Indication")

  if (is.na(xml_attr(use_nodeset[[1]], "use_nr"))) {
    stop("You need to add use numbers with srppp_xml_use_numbers() first")
  }

  get_use <- function(node) {
    wNbr <- xml_attr(xml_parent(xml_parent(node)), "wNbr")
    attributes <- xml_attrs(node)
    units_pk <- xml_attr(xml_child(node, search = 1), "primaryKey")

    # Searching for a child node e.g. with time units by name is too slow
    #time_units_pk <- xml_attr(xml_child(node, search = "TimeMeasure"), "primaryKey")

    ret <- c(wNbr,
      attributes[c(
        "dosageFrom", "dosageTo",
        "waitingPeriod",
        "expenditureForm", "expenditureTo",
        "use_nr")],
      units_pk)

    names(ret) <- c("wNbr",
      "min_dosage", "max_dosage",
      "waiting_period",
      "min_rate", "max_rate",
      "use_nr", "units_pk")

    return(ret)
  }

  rate_unit_descriptions <- description_table(srppp_xml, "Measure") |>
    rename(units_de = de, units_fr = fr) |>
    rename(units_it = it, units_en = en) |>
    rename(units_pk = desc_pk) |>
    arrange(units_pk)

  time_units_nodeset <- xml_find_all(srppp_xml, "Products/Product/ProductInformation/Indication/TimeMeasure")
  get_time_units <- function(node) {
    wNbr <- xml_attr(xml_parent(xml_parent(xml_parent((node)))), "wNbr")
    use_nr <- xml_attr(xml_parent(node), "use_nr")
    time_units_pk <- xml_attr(node, "primaryKey")
    ret <- (c(wNbr, use_nr, time_units_pk))
    names(ret) <- c("wNbr", "use_nr", "time_units_pk")
    return(ret)
  }
  time_units <- t(sapply(time_units_nodeset, get_time_units)) |>
    as_tibble() |>
    mutate(use_nr = as.integer(use_nr))

  time_unit_descriptions <- srppp_xml |>
    xml_find_all(paste0("MetaData[@name='TimeMeasure']/Detail")) |>
    sapply(get_descriptions, code = FALSE) |> t() |>
    as_tibble() |>
    rename(time_units_de = de, time_units_fr = fr) |>
    rename(time_units_it = it, time_units_en = en) |>
    rename(time_units_pk = desc_pk) |>
    arrange(time_units_pk)

  uses <- t(sapply(use_nodeset, get_use)) |>
    as_tibble() |>
    mutate_at(c("min_dosage", "max_dosage", "min_rate", "max_rate"), as.numeric) |>
    mutate_at(c("waiting_period", "use_nr"), as.integer) |>
    select(wNbr, use_nr, min_dosage, max_dosage, waiting_period, min_rate, max_rate, units_pk) |>
    left_join(time_units, by = join_by(wNbr, use_nr))

  ret <- uses |>
    left_join(rate_unit_descriptions, by = "units_pk") |>
    left_join(time_unit_descriptions, by = "time_units_pk") |>
    select(wNbr, use_nr, ends_with("dosage"),
      ends_with("rate"), starts_with("units"), -units_pk,
      waiting_period, starts_with("time_units"), -time_units_pk,
      starts_with("application_area")) |>
    arrange(wNbr, use_nr)

  return(ret)
}

#' Create a dm object from an XML version of the Swiss Register of Plant Protection Products
#'
#' While reading in the data, the information obtained from the XML file is
#' left unchanged, with the exceptions listed in the section 'Details'.
#' An overview of the contents of the most important tables in the resulting
#' data object is given in `vignette("srppp")`.
#'
#' @details
#'
#' ## Corrections made to the data
#'
#' - In the following case, the product composition is corrected while reading in
#' the data: The active substance content of Dormex (W-3066) is not 667 g/L,
#' but 520 g/L This was confirmed by a visit to the Wädenswil archive by
#' Johannes Ranke and Daniel Baumgartner, 2024-03-27.
#'
#' ## Removal of redundant information
#'
#' - Information on products that has been duplicated across several products
#' sharing the same P-Number has been associated directly with this P-Number,
#' in order to avoid duplications. While reading in the XML file, it is checked
#' that the resulting deduplication does not remove any data.
#' - In very few cases of historical XML files, there are two `<Product>`
#' sections sharing the same W-Number. In these cases, one of these has apparently
#' been included in error and an informed decision is taken while reading in
#' the data which one of these sections is discarded. The details of this
#' procedure can be found in the source code of the function
#' `srppp_xml_get_products`.
#'
#' ## Amendments to the data
#'
#' In the table of obligations, the following information on mitigation
#' measures is extracted from the ones relevant for the environment (SPe 3).
#' - "sw_drift_dist": Unsprayed buffer towards surface waters to mitigate
#'   spray drift in meters
#' - "sw_runoff_dist": Vegetated buffer towards surface waters to mitigate
#'   runoff in meters
#' - "sw_runoff_points": Required runoff mitigation points to mitigate runoff
#' - "biotope_drift_dist": Unsprayed buffer towards biotopes (as defined in
#'    articles 18a and 18b of the Federal Act on the Protection of Nature and
#'    Cultural Heritage) to mitigate spray drift in meters
#'
#' @param from A specification of the way to retrieve the XML to be passed to
#' [srppp_xml_get], or an object of the class 'srppp_xml'
#' @param remove_duplicates Should duplicates based on wNbrs be removed?
#' @param verbose Should we give some feedback?
#' @return A [dm::dm] object with tables linked by foreign keys
#' pointing to primary keys, i.e. with referential integrity.
#' Since version 1.1, the returned object has an attribute named 'culture_tree'
#' of class [data.tree::Node].
#' @export
#' @examples
#' \donttest{ # Avoid NOTE on CRAN caused by checks >5s
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
#' dm_examine_constraints(sr)
#' dm_draw(sr)
#'
#' # Show ingredients for products named 'Boxer'
#' sr$products |>
#'   filter(name == "Boxer") |>
#'   left_join(sr$ingredients, by = "pNbr") |>
#'   left_join(sr$substances, by = "pk") |>
#'   select(wNbr, name, pNbr, isSalePermission, substance_de, g_per_L)
#'
#' # Show authorised uses of the original product
#' boxer_uses <- sr$products |>
#'   filter(name == "Boxer", !isSalePermission) |>
#'   left_join(sr$uses, by = "pNbr") |>
#'   select(pNbr, use_nr,
#'     min_dosage, max_dosage, min_rate, max_rate, units_de,
#'     waiting_period, time_units_de, application_area_de)
#' print(boxer_uses)
#'
#' # Show crop for use number 1
#' boxer_uses |>
#'   filter(use_nr == 1) |>
#'   left_join(sr$cultures, join_by(pNbr, use_nr)) |>
#'   select(use_nr, culture_de)
#'
#' # Show target pests for use number 1
#' boxer_uses |>
#'   filter(use_nr == 1) |>
#'   left_join(sr$pests, join_by(pNbr, use_nr)) |>
#'   select(use_nr, pest_de)
#'
#' # Show obligations for use number 1
#' boxer_uses |>
#'   filter(use_nr == 1) |>
#'   left_join(sr$obligations, join_by(pNbr, use_nr)) |>
#'   select(use_nr, sw_runoff_points, obligation_de) |>
#'   knitr::kable() |>
#'   print()
#'
#' # Show application comments for use number 1
#' boxer_uses |>
#'   filter(use_nr == 1) |>
#'   left_join(sr$application_comments, join_by(pNbr, use_nr)) |>
#'   select(use_nr, application_comment_de)
#'
#' # Illustrate 'obligations' indicating varying effects
#' sr$obligations |>
#'   filter(varying_effect) |>
#'   select(pNbr, use_nr, code, obligation_de)
#'
#' }
srppp_dm <- function(from = srppp_xml_url, remove_duplicates = TRUE, verbose = TRUE) {

  if (inherits(from, "srppp_xml")) {
    srppp_xml <- from
    from <- attr(srppp_xml, "from")
  } else {
    srppp_xml <- srppp_xml_get(from)
  }

  # Tables of products and associated information
  # Duplicates were already removed from the XML, if requested
  products <- srppp_xml_get_products(srppp_xml, remove_duplicates = remove_duplicates,
    verbose = verbose)
  pNbrs <- tibble(pNbr = as.integer(unique(products$pNbr))) |>
    arrange(pNbr)

  parallel_imports <- srppp_xml_get_parallel_imports(srppp_xml)

  product_information_table <- function(srppp_xml, tag_name, prefix = tag_name, code = FALSE) {
    descriptions <- description_table(srppp_xml, tag_name, code = code)

    if (identical(descriptions, NA)) { # no signal words in old XML files
      ret <- tibble(pNbr = integer(0))
    } else {
      product_information_nodes <- xml_find_all(srppp_xml,
        paste0("Products/Product/ProductInformation/", tag_name))

      ret <- tibble(
          wNbr = sapply(product_information_nodes, get_grandparent_wNbr),
          desc_pk = xml_attr(product_information_nodes, "primaryKey")
        ) |>
        filter(!grepl("-", wNbr)) |>
        left_join(descriptions, by = "desc_pk") |>
        rename_with(function(colname) paste(prefix, colname, sep = "_"),
          .cols = c(de, fr, it, en)) |>
        left_join(products[c("pNbr", "wNbr")], by = "wNbr") |>
        select(pNbr, !c(pNbr, wNbr)) |>
        arrange(pNbr)
    }

    return(ret)
  }

  product_categories <- unique( # ProductCategory tags are often duplicated in the XML files
    product_information_table(srppp_xml, "ProductCategory", prefix = "category"))
  formulation_codes <- product_information_table(srppp_xml, "FormulationCode", prefix = "formulation")
  danger_symbols <- product_information_table(srppp_xml, "DangerSymbol", code = TRUE)
  signal_words <- product_information_table(srppp_xml, "SignalWords", prefix = "signal")
  CodeS <- product_information_table(srppp_xml, "CodeS", code = TRUE)
  CodeR <- product_information_table(srppp_xml, "CodeR", code = TRUE)
  # Permission holder was skipped, as we will probably not need this information

  # Tables of product ingredients and their concentrations
  substances <- srppp_xml_get_substances(srppp_xml)
  ingredients_no_pNbr <- srppp_xml_get_ingredients(srppp_xml)
  ingredients_with_dups <- ingredients_no_pNbr |>
    left_join(products[c("wNbr", "pNbr")], by = "wNbr") |>
    select(pNbr, pk, type, percent, g_per_L, ingredient_de, ingredient_fr, ingredient_it) |>
    arrange(pNbr, pk, type)

  # Several duplicate entries for ingredients are removed below (see github issue #6)

  # - In 2017 and 2018, there are duplicate ingredient entries for oryzalin (pk 276)
  # in the product Surflan (P-Number 4711). The entry with 48 percent is apparently
  # wrong, as in the last occurrence of the product in 2022, 40.5 percent are specified
  # That also makes sense, because 48 percent would mean a density of exactly 1 L/kg
  # (the concentration is 480 g/L) which is unlikely.
  # - Product with P-Number 8755 contains an erroneous duplicated ingredient
  # entry for active ingredient 950 in 2019 and 2020, with an entry "70.9 g / Dose"
  # in 'ingredients_de'.
  # - Completely equal lines for P-Numbers 8122 and 3562

  ingredients <- ingredients_with_dups |>
    filter(!(pNbr == 4711 & pk == 276 & percent == 48)) |>
    filter(!(pNbr == 8755 & pk == 950 & ingredient_de == "70.9 g / Dose")) |>
    unique() # for P-Numbers 8122 and 3562

  ingredient_dups <- ingredients |>
    group_by(pNbr, pk) |>
    summarise(n = n(), .groups = "drop") |>
    filter(n > 1)
  if (nrow(ingredient_dups) > 0) {
    print(ingredient_dups)
    stop("Duplicate entries for the same ingredient in the same product")
  }

  # Define use IDs (attribute 'use_nr' in the XML tree)
  srppp_xml <- srppp_xml_define_use_numbers(srppp_xml)

  indication_information_table <- function(srppp_xml,
    tag_name, additional_text = FALSE, type = FALSE)
  {

    indication_information_nodes <- xml_find_all(srppp_xml,
      paste0("Products/Product[not(contains(@wNbr, '-'))]/ProductInformation/Indication/", tag_name))

    ret <- tibble(
      wNbr = sapply(indication_information_nodes, get_great_grandparent_wNbr),
      use_nr = sapply(indication_information_nodes, get_use_nr),
      desc_pk = xml_attr(indication_information_nodes, "primaryKey")) |>
        mutate(use_nr = as.integer(use_nr)) |>
        left_join(products[c("wNbr", "pNbr")], by = "wNbr") |>
        select(pNbr, !c(wNbr, pNbr))

    if (additional_text) {
      ret$add_txt_pk <- xml_attr(indication_information_nodes,
        "additionalTextPrimaryKey")
    }

    if (type) {
      ret$type <- xml_attr(indication_information_nodes, "type")
    }

    return(ret)
  }

  application_area_descriptions <- description_table(srppp_xml, "ApplicationArea")
  application_areas <- indication_information_table(srppp_xml, "ApplicationArea") |>
    left_join(application_area_descriptions, by = "desc_pk") |>
    rename(application_area_de = de, application_area_fr = fr) |>
    rename(application_area_it = it, application_area_en = en) |>
    select(-desc_pk)

  # Table of uses ('indications') and associated information tables
  uses <- srppp_xml_get_uses(srppp_xml) |>
    left_join(products[c("wNbr", "pNbr")], by = "wNbr") |>
    left_join(application_areas, by = c("pNbr", "use_nr")) |>
    select(pNbr, !c(pNbr, wNbr))

  # Check that we have exactly one application area per use
  problem_uses <- uses |>
    group_by(pNbr, use_nr) |>
    summarise(n = n(), .groups = "drop_last") |>
    filter(n != 1)

  if (nrow(problem_uses) > 0) {
    cli::cli_abort("Assumption of 1 application area per use is violated")
  }

  application_comment_descriptions <- description_table(srppp_xml, "ApplicationComment")
  application_comments <- indication_information_table(srppp_xml, "ApplicationComment") |>
    left_join(application_comment_descriptions, by = "desc_pk") |>
    rename(application_comment_de = de, application_comment_fr = fr) |>
    rename(application_comment_it = it, application_comment_en = en) |>
    select(-desc_pk)

  # Sometimes we have one or more specific culture form(s) in the use definition
  culture_form_descriptions <- description_table(srppp_xml, "CultureForm")
  culture_forms <- indication_information_table(srppp_xml, "CultureForm") |>
    left_join(culture_form_descriptions, by = "desc_pk") |>
    rename(culture_form_de = de, culture_form_fr = fr) |>
    rename(culture_form_it = it, culture_form_en = en) |>
    select(-desc_pk) |>
    arrange(pNbr, use_nr)

  # In the culture descriptions, links to parent cultures are considered
  culture_descriptions <- description_table(srppp_xml, "Culture", parent_keys = TRUE)
  culture_additional_texts <- description_table(srppp_xml, "CultureAdditionalText")
  cultures <- indication_information_table(srppp_xml, "Culture", additional_text = TRUE) |>
    left_join(culture_descriptions, by = "desc_pk") |>
    rename(culture_de = de, culture_fr = fr) |>
    rename(culture_it = it, culture_en = en) |>
    left_join(culture_additional_texts, c(add_txt_pk = "desc_pk")) |>
    rename(culture_add_txt_de = de, culture_add_txt_fr = fr) |>
    rename(culture_add_txt_it = it, culture_add_txt_en = en) |>
    select(-desc_pk, -add_txt_pk) |>
    arrange(pNbr, use_nr)

  culture_tree <- build_culture_tree(culture_descriptions)

  pest_descriptions <- description_table(srppp_xml, "Pest", latin = TRUE)
  pest_additional_texts <- description_table(srppp_xml, "PestAdditionalText")
  pests <- indication_information_table(srppp_xml, "Pest",
    additional_text = TRUE, type = TRUE) |>
    left_join(pest_descriptions, by = "desc_pk") |>
    rename(pest_de = de, pest_fr = fr) |>
    rename(pest_it = it, pest_en = en) |>
    left_join(pest_additional_texts, c(add_txt_pk = "desc_pk")) |>
    rename(pest_add_txt_de = de, pest_add_txt_fr = fr) |>
    rename(pest_add_txt_it = it, pest_add_txt_en = en) |>
    select(-desc_pk, -add_txt_pk) |>
    arrange(pNbr, use_nr)

  obligation_descriptions <- description_table(srppp_xml, "Obligation", code = TRUE)
  obligations <- indication_information_table(srppp_xml, "Obligation") |>
    left_join(obligation_descriptions, by = "desc_pk") |>
    mutate(across(c(de, fr, it, en), trimws)) |>
    rename(obligation_de = de, obligation_fr = fr) |>
    rename(obligation_it = it, obligation_en = en) |>
    select(-desc_pk) |>
    arrange(pNbr, use_nr)

  obligations_spe3_pest_partial_effect <- obligations |>
    mutate(
      sw_drift_dist = case_when( # Unsprayed buffer towards surface waters
        grepl("Mindestabstand von [0-9]{1,3} m zu einem Oberfl\u00e4chengew\u00e4sser",
          obligation_de) ~ sub(
            ".*Mindestabstand von ([0-9]{1,3}) m zu einem Oberfl\u00e4chengew\u00e4sser.*",
            "\\1",
            obligation_de),
        grepl("unbehandelte Pufferzone von [0-9]{1,3} m zu Oberfl\u00e4chengew\u00e4ssern",
          obligation_de) ~ sub(
            ".*unbehandelte Pufferzone von ([0-9]{1,3}) m zu Oberfl\u00e4chengew\u00e4ssern.*",
            "\\1",
            obligation_de),
        .default = NA),
      sw_runoff_dist = case_when( # Vegetated buffer towards surface waters
        grepl("Gew\u00e4sserorganismen.*Abschwemmung.*eine unbehandelte Pufferzone",
          obligation_de) ~ sub(
            ".*unbehandelte Pufferzone von ([0-9]{1,3}) m.*",
            "\\1",
            obligation_de),
        grepl("Gew\u00e4sserorganismen.*Abschwemmung.*bewachsene Pufferzone",
          obligation_de) ~ sub(
            ".*mit einer geschlossenen Pflanzendecke bewachsene Pufferzone von mindestens ([0-9]{1,3}) m.*",
            "\\1",
            obligation_de),
        grepl("Gew\u00e4sserorganismen.*Abschwemmung.*bewachsene unbehandelte Pufferzone",
          obligation_de) ~ sub(
            ".*bewachsene unbehandelte Pufferzone von (?:mindestens )?([0-9]{1,3}) m.*",
            "\\1",
            obligation_de),
        .default = NA),
      sw_runoff_points = case_when( # Required runoff mitigation points
        grepl("Gew\u00e4sserorganismen.*Abschwemmung.*Punkt",
          obligation_de) ~ sub(
            ".*([0-9]) Punkt.*",
            "\\1",
            obligation_de),
        .default = NA),
      biotope_drift_dist = case_when( # Unsprayed buffer towards biotopes
        grepl("unbehandelte Pufferzone von [0-9]{1,3} m zu Biotopen",
          obligation_de) ~ sub(
            ".*unbehandelte Pufferzone von ([0-9]{1,3}) m zu Biotopen.*",
            "\\1",
            obligation_de),
        .default = NA)
    ) |>
    mutate(across(
      c(sw_drift_dist, sw_runoff_dist, sw_runoff_points, biotope_drift_dist),
      as.integer)) |>
    mutate(
      varying_effect = case_when(
        grepl("Die Wirkungseffizienz der N\u00fctzlinge kann je nach Pflanzenart stark schwanken", obligation_de) ~ TRUE,
        grepl("Die Wirkungseffizienz dieses Produkt wurde nicht in allen Kulturen und f\u00fcr alle Applikationen gepr\u00fcft und kann deshalb je nach Kultur, Substrat oder Applikationsbedingungen stark schwanken.", obligation_de) ~ TRUE,
        grepl("Die Wirkungseffizienz dieses Produkts wurde nicht f\u00fcr alle Applikationen gepr\u00fcft und kann deshalb je nach Substrat oder Applikationsbedingungen stark schwanken.", obligation_de) ~ TRUE,
        .default = FALSE
      )
    )



  srppp_dm <- dm(products,
    pNbrs,
    categories = product_categories,
    formulation_codes,
    danger_symbols, CodeS, CodeR, signal_words,
    parallel_imports,
    substances, ingredients,
    uses, application_comments,
    culture_forms, cultures, pests,
    obligations = obligations_spe3_pest_partial_effect) |>
    dm_add_pk(products, wNbr) |>
    dm_add_pk(pNbrs, pNbr) |>
    dm_add_pk(parallel_imports, id) |>
    dm_add_pk(substances, pk) |>
    dm_add_pk(uses, c(pNbr, use_nr)) |>
    dm_add_fk(products, pNbr, pNbrs) |>
    dm_add_fk(categories, pNbr, pNbrs) |>
    dm_add_fk(formulation_codes, pNbr, pNbrs) |>
    dm_add_fk(danger_symbols, pNbr, pNbrs) |>
    dm_add_fk(CodeS, pNbr, pNbrs) |>
    dm_add_fk(CodeR, pNbr, pNbrs) |>
    dm_add_fk(signal_words, pNbr, pNbrs) |>
    dm_add_fk(ingredients, pNbr, pNbrs) |>
    dm_add_fk(ingredients, pk, substances) |>
    dm_add_fk(uses, pNbr, pNbrs) |>
    dm_add_fk(application_comments, c(pNbr, use_nr), uses) |>
    dm_add_fk(culture_forms, c(pNbr, use_nr), uses) |>
    dm_add_fk(cultures, c(pNbr, use_nr), uses) |>
    dm_add_fk(pests, c(pNbr, use_nr), uses) |>
    dm_add_fk(obligations, c(pNbr, use_nr), uses) |>
    dm_add_fk(parallel_imports, pNbr, pNbrs) |>
    dm_set_colors(
      darkblue = c(pNbrs:signal_words),
      lightblue = c(products, parallel_imports),
      darkorange = c(ingredients, substances),
      darkgreen = uses:obligations)

    attr(srppp_dm, "from") <- attr(srppp_xml, "from")
    attr(srppp_dm, "culture_tree") <- culture_tree
    attr(srppp_dm, "srppp_xml_version") <- attr(srppp_xml, "srppp_xml_version")
    class(srppp_dm) <- c("srppp_dm", "dm")
    return(srppp_dm)
}

#' @rdname srppp_dm
#' @param x A [srppp_dm] object
#' @param \dots Not used
#' @export
print.srppp_dm <- function(x, ...) {
  cat("<srppp_dm> object from:", attr(x, "from"), "\n")
  print(dm::dm_nrow(x))
  invisible(x)
}

#' Clean product names
#'
#' @param names Character vector of product names that should be cleaned from comments
#' @return Character vector of cleaned names
#' @export
srppp_xml_clean_product_names <- function(names) {
  names |>
    trimws() |>
    stringr::str_remove(" \\(Bew. suspendiert.*\\)$") |>
    stringr::str_remove(" \\(Bew. beendet.*\\)$") |>
    stringr::str_remove(" \\[Bewilligung beendet.*\\]$") |>
    stringr::str_remove(" Bewilligung beendet.*") |>
    stringr::str_remove(" \\[Die Verwendung der Charge.*$") |>
    stringr::str_remove(" \\[Erneuerungsgesuch in Bearbeitung\\]$") |>
    stringr::str_remove(" \\[Erneuerung in Bearbeitung\\]$") |>
    stringr::str_remove(" \\(Erneuerungsgesuch in Bearbeitung\\)$") |>
    stringr::str_remove(" \\[deman[cd]e de renouvellement en cours\\]$") |>
    stringr::str_remove(" \\(demande de renouvellement en cours\\)$") |>
    stringr::str_remove(" \\[Ausverkaufs.*frist.*\\]$") |>
    stringr::str_remove(" \\[Aufbrauch.*frist.*\\]$") |>
    stringr::str_remove(" \\[Wegen h.ngigem.*\\]$")
}

#' Get W-Number from parent node
#' @keywords internal
get_parent_wNbr <- function(node) {
  xml_attr(xml_parent(node), "wNbr")
}

#' Get W-Number from grandparent node
#' @keywords internal
get_grandparent_wNbr <- function(node) {
  xml_attr(xml_parent(xml_parent(node)), "wNbr")
}

#' Get W-Number from grandparent node
#' @keywords internal
get_great_grandparent_wNbr <- function(node) {
  xml_attr(xml_parent(xml_parent(xml_parent(node))), "wNbr")
}

#' Get use number from parent node (indication information node)
#' @keywords internal
get_use_nr <- function(node) {
  xml_attr(xml_parent(node), "use_nr")
}

#' Get a table of descriptions for a certain Meta Information Tag
#' @keywords internal
description_table <- function(srppp_xml, tag_name, code = FALSE, latin = FALSE, parent_keys = FALSE) {

  # Find nodes and apply the function
  nodes <- srppp_xml |>
    xml_find_all(paste0("MetaData[@name='", tag_name, "']/Detail"))

  if (length(nodes) > 0) {
    ret <- nodes |>
      sapply(get_descriptions, code = code, latin = latin, parent_keys = parent_keys) |> t() |>
      as_tibble() |>
      arrange(desc_pk)
  } else {
    ret <- NA
  }

  return(ret)
}

#' Get descriptions from a node with children that hold descriptions
#' @keywords internal
#' @param node The node to look at
#' @param code Do the description nodes have a child holding a code?
#' @param latin Are there latin descriptions (e.g. for pest descriptions)
#' @param parent_keys For culture descriptions, we also return up to two primary keys
#' that link to parent cultures
get_descriptions <- function(node, code = FALSE, latin = FALSE, parent_keys = FALSE) {
  desc_pk <- xml_attr(node, "primaryKey")
  desc <- trimws(sapply(xml_children(node), xml_attr, "value"))
  names(desc) <- sapply(xml_children(node), xml_attr, "language")
  if (code) {
    if (xml_length(xml_child(node)) == 1) {
      code <- xml_attr(xml_child(xml_child(node)), "value")
    } else {
      code <- NA
    }
    ret <- c(desc_pk, code, desc[c("de", "fr", "it", "en")])
    names(ret) <- c("desc_pk", "code", "de", "fr", "it", "en")
    return(ret)
  } else {
    desc <- desc[!is.na(desc)] # Remove NA results that we get from <Parent> nodes without "value"
    if (parent_keys) {
      # Only children that are <Parent> nodes have attributes "primaryKey"
      pks <- sapply(xml_children(node), xml_attr, "primaryKey")
      pks <- pks[!is.na(pks)] # Remove NA results
      parent_key_1 <- NA
      parent_key_2 <- NA
      if (length(pks) > 0) parent_key_1 <- pks[1]
      if (length(pks) > 1) parent_key_2 <- pks[2]
      if (length(pks) > 2) stop("Assumption of a maximum of two parent primary keys for cultures appears to be wrong")
      ret <- c(desc_pk, desc[c("de", "fr", "it", "en")], parent_key_1, parent_key_2)
      names(ret) <- c("desc_pk", "de", "fr", "it", "en", "prt_1_pk", "prt_2_pk")
      return(ret)
    } else {

      if (latin) {
        ret <- c(desc_pk, desc[c("de", "fr", "it", "en", "lt")])
        names(ret) <- c("desc_pk", "de", "fr", "it", "en", "lt")
      } else {
        ret <- c(desc_pk, desc[c("de", "fr", "it", "en")])
        names(ret) <- c("desc_pk", "de", "fr", "it", "en")
      }
      return(ret)
    }
  }
}

