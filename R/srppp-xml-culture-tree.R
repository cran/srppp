utils::globalVariables(c("prt_1_pk", "prt_2_pk"))
#' Build a Culture Tree
#'
#' Constructs a hierarchical tree structure from a culture description table
#' that is created within the [srppp_dm] function. As each culture can
#' have one or two parent nodes in an srppp XML file, the nodes with
#' two parent nodes are duplicated. The duplicated nodes retain their
#' primary key as an attribute, so the information on their identity
#' does not get lost.
#'
#' @importFrom data.tree Node
#' @param culture_descriptions A tibble containing culture descriptions with the following columns:
#'   - `desc_pk`: Unique identifier for each culture node.
#'   - `de`: Culture name in German.
#'   - `fr`: Culture name in French.
#'   - `it`: Culture name in Italian.
#'   - `en`: Culture name in English.
#'   - `prt_1_pk`: Identifier of the first parent node (can be NA if no parent).
#'   - `prt_2_pk`: Identifier of the second parent node (can be NA if no second parent).
#'
#' @return A [data.tree::Node] representing the root of the
#' culture hierarchy. Each node in the tree has the following attributes:
#'   - `name_de`: The German name of the culture (from the `de` column).
#'   - `name_fr`: The French name of the culture (from the `fr` column).
#'   - `name_it`: The Italian name of the culture (from the `it` column).
#'   - `culture_id`: The unique identifier of the culture
#'
#' @details
#' The function builds the culture tree in two main steps:
#' 1. Node Creation: It first creates all unique culture nodes and adds them to a lookup environment.
#'    Each node is initialized with its German name and its `culture_id`.
#' 2. Relationship Establishment: It then establishes parent-child relationships between nodes.
#'    Any node that has a second parent culture is duplicated and the duplicate
#'    is associated with the second parent culture
#'
#' @keywords internal
build_culture_tree <- function(culture_descriptions) {

  root <- Node$new("Cultures")
  node_lookup <- new.env(hash = TRUE)

  # Select columns to use
  df <- culture_descriptions |>
    select(desc_pk, de, fr, it, prt_1_pk, prt_2_pk) |>
    unique()

  # Create all nodes once, add culture_id and names
  for (i in 1:nrow(df)) {
    if (is.null(node_lookup[[as.character(df$desc_pk[i])]])) {
      new_node <- Node$new(df$de[i])
      new_node$name_de <- df$de[i]
      new_node$name_fr <- df$fr[i]
      new_node$name_it <- df$it[i]
      new_node$culture_id <- df$desc_pk[i]
      new_node$is_duplicate <- FALSE
      node_lookup[[as.character(df$desc_pk[i])]] <- new_node
    }
  }

  # Establish parent-child relationships
  for (i in 1:nrow(df)) {
    child_node <- node_lookup[[as.character(df$desc_pk[i])]]

    add_parent <- function(parent_key) {
      if (!is.na(parent_key)) {
        parent_node <- node_lookup[[as.character(parent_key)]]
        if (!is.null(parent_node)) {
          if (is.null(child_node$parent)) {
            parent_node$AddChildNode(child_node)
          } else if (!identical(child_node$parent, parent_node)) {
            # If the child already has a different parent,
            # create a new child node and add names
            dup_node <- Node$new(paste0(child_node$name, " [dup]"))
            dup_node$is_duplicate <- TRUE
            dup_node$culture_id <- df$desc_pk[i]
            dup_node$name_de <- child_node$name_de
            dup_node$name_fr <- child_node$name_fr
            dup_node$name_it <- child_node$name_it
            dup_node$reference_to <- child_node$culture_id
            parent_node$AddChildNode(dup_node)
          }
        }
      }
    }

    add_parent(df$prt_1_pk[i])
    add_parent(df$prt_2_pk[i])

    if (is.null(child_node$parent)) {
      root$AddChildNode(child_node)
    }
  }

  # Lookup table for parent child relationships
  parent_child_df <- data.frame(
    parent = character(),
    child = character(),
    stringsAsFactors = FALSE)

  extract_parent_child <- function(node) {
    if (!is.null(node$parent)) {
      parent_name <- node$parent$name
      child_name <- node$name

      parent_child_df <<- rbind(
        parent_child_df,
        data.frame(
          parent = parent_name,
          child = gsub(" \\[dup\\]$", "", child_name),
          stringsAsFactors = FALSE
        )
      )
    }

    for (child in node$children) {
      extract_parent_child(child)
    }
  }

  extract_parent_child(root)

  # Helper function to get all descendants of a culture
  get_all_descendants <- function(culture) {
    descendants <- culture
    children <- parent_child_df$child[parent_child_df$parent == culture]

    while (length(children) > 0) {
      descendants <- c(descendants, children)
      children <- unique(unlist(lapply(children, function(x) parent_child_df$child[parent_child_df$parent == x])))
      children <- children[!children %in% descendants]
    }

    return(unique(descendants))
  }


  # Get leaf nodes (cultures with no children)
  leaf_nodes <- setdiff(parent_child_df$child, parent_child_df$parent)

  # Remove leaf nodes that contain "allg.", because they should not be leaf nodes
  leaf_nodes_allg <- leaf_nodes[grepl("allg.", leaf_nodes, ignore.case = TRUE)]
  leaf_nodes <- leaf_nodes[!grepl("allg.", leaf_nodes, ignore.case = TRUE)]

  culture_leaf_df <- data.frame(
    culture = character(),
    leaf_culture = character(),
    stringsAsFactors = FALSE)

  for (culture_de in df$de) {

    # Make sure the current culture is not one of the removed leaf nodes
    if (!(culture_de %in% leaf_nodes_allg)) {

      # Get all descendants of the culture
      all_descendants <- get_all_descendants(culture_de)

      # Filter for leaf nodes among descendants
      leaf_cultures_de <- intersect(all_descendants, leaf_nodes)

      new_culture_leaf_data <- data.frame(
        culture_de = culture_de,
        leaf_culture_de = leaf_cultures_de,
        stringsAsFactors = FALSE)

      culture_leaf_df <- rbind(culture_leaf_df, new_culture_leaf_data)

    }
  }

  attr(root, "culture_leaf_df") <- culture_leaf_df


  return(root)
}
