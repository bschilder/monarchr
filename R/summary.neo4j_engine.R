#' Summarize contents of a Neo4j KG engine
#'
#' Given a Neo4j based KG engine, provides summary information in the form of
#' node counts, category counts across nodes, relationship type counts, and
#' available properties.
#' The returned summary object prints a readable console report and also
#' contains
#' data frames with this information. Also returned are `cats`, `preds`, and
#' `props` entries, containing lists of available
#' categories/predicates/properties for convenient auto-completion in RStudio.
#'
#' @param object A `neo4j_engine` object
#' @param ... Other parameters (not used)
#' @param quiet Logical, whether to suppress printing of the summary
#' @return A classed list of data frames and named lists.
#' @export
#' @examplesIf FALSE
#' # prints a readable summary and returns a list of dataframes
#' stats <- monarch_engine() |> summary()
#' print(stats)
#'
summary.neo4j_engine <- function(object, ..., quiet = FALSE) {
    node_summary_df <- cypher_query_df(object, "MATCH (n) UNWIND labels(n) AS category WITH category, COUNT(n) AS count RETURN category, count ORDER BY count DESC")
    edge_summary_df <- cypher_query_df(object, "MATCH ()-[r]->() RETURN type(r) AS predicate, COUNT(*) AS count ORDER BY count DESC")

    counts_query <- "
        // Count the total number of nodes
        MATCH (n)
        RETURN 'nodes_total' AS Type, COUNT(n) AS Count
        UNION
        // Count the total number of edges
        MATCH ()-[r]->()
        RETURN 'edges_total' AS Type, COUNT(r) AS Count
        "

    total_df <- cypher_query_df(object, counts_query)
    total_nodes <- total_df$Count[1]
    total_edges <- total_df$Count[2]

    properties <- cypher_query_df(object, "CALL db.propertyKeys()")$propertyKey


    cats <- as.list(node_summary_df$category)
    names(cats) <- cats

    preds <- as.list(edge_summary_df$predicate)
    names(preds) <- preds

    props <- as.list(properties)
    names(props) <- props

    res <- list(
        node_summary = node_summary_df,
        edge_summary = edge_summary_df,
        total_nodes = total_nodes,
        total_edges = total_edges,
        cats = cats,
        preds = preds,
        props = props
    )

    class(res) <- c("summary.neo4j_engine", "summary_monarchr", class(res))

    if (quiet) {
        return(invisible(res))
    }

    return(res)
}


#' Print a neo4j-engine summary
#'
#' @param x A `summary.neo4j_engine` object.
#' @param ... Other parameters (not used).
#' @return Invisibly returns `x`.
#' @export
print.summary.neo4j_engine <- function(x, ...) {
    format_table <- function(df) {
        capture.output(noquote(format(df, row.names = FALSE)))
    }

    writeLines(c(
        "",
        "A Neo4j-backed knowledge graph engine.",
        paste0("Total nodes: ", x$total_nodes),
        paste0("Total edges: ", x$total_edges),
        "",
        "Node category counts:"
    ))
    writeLines(format_table(x$node_summary))
    writeLines(c("", "Edge type counts:"))
    writeLines(format_table(x$edge_summary))
    writeLines(c("", "Available node and edge properties:"))
    writeLines(capture.output(unname(unlist(x$props))))
    writeLines(c(
        "",
        "For more information about Biolink node (Class) and edge",
        "(Association) properties, see https://biolink.github.io/biolink-model/."
    ))

    return(invisible(x))
}
