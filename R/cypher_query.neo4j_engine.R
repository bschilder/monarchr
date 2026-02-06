########### Private functions ###########


#' @noRd
stitch_vectors <- function(x) {
    # Neo4j array results come back as lists of length-1 character vectors;
    # these need to be stitched together to length-N character vectors.
    # We leave these in a list element to distinguish sets of size 1 from
    # scalar strings

    # Check if the element is a list
    if (is.list(x)) {
        # Check if all elements of the list are length-1 character vectors
        if (all(unlist(lapply(x, function(y) is.character(y) && length(y) == 1)))) {
            # Concatenate all single-character vectors and put them in a list
            return(list(unlist(x)))
        } else {
            # Recursively apply the function to each element of the list
            return(lapply(x, stitch_vectors))
        }
    } else {
        # Return the element as is if it's not a list
        return(x)
    }
}

#' Remove names from columns
#'
#' Given a data-frame like object, runs each column through unname()
#'
#' @param df Input data frame
#' @importFrom rlang is_named
#' @return The input, with unnamed columns
unname_cols <- function(df) {
    for (name in names(df)) {
        df[[name]] <- unname(df[[name]])
    }

    return(df)
}

#' Process neo2R cypher to tbl_kgx
#'
#' Given a result from neo2R::cypher returning KGX-formatted nodes and edges,
#' parse the result to generate a tbl_kgx object, attaching the provided engine.
#'
#' @param res The result from neo2R::cypher with result = "graph"
#' @param engine The engine to attach to the returned graph
#' @importFrom memoise memoise
#' @importFrom rlang is_named
#' @return A tbl_kgx
neo2r_to_kgx <- function(res, engine) {
    relationship_ids_contained <- as.integer(unlist(res$paths))

    if (!is.null(res)) {
        res <- stitch_vectors(res)
    }

    ## NOTE: in cases where there are no array properties on a node,
    ## stitch_vectors above will result in a *named vector* in res$nodes[[x]]$properties
    # the code below does not account for that
    # (this was previously unseen as all nodes had a multivalued `category` property)

    # this could be more of an issue with edges below; even though there should be no edges without a subject, predicate, and object,
    # it is possible that some edges may not have any other properties, and so the code below assuming edge properties are lists may fail, even in a well-formed KGX graph


    ## node info
    node_ids <- unlist(lapply(res$nodes, function(node) {
        node$properties$id
    }))

    node_categories <- lapply(res$nodes, function(node) {
        node$properties$category[[1]] ## pull these out of the list container to get a simple list of vecs
    })

    nodes_df <- tibble::tibble(id = node_ids, category = node_categories)

    ## compute a pcategory, or priority category, based on a preference list
    prefs <- engine$preferences
    nodes_df$pcategory <- normalize_categories(node_categories, prefs$category_priority)

    ## add all other node properties as columns
    node_prop_names <- unname(unique(unlist(lapply(res$nodes, function(node) {
        names(node$properties)
    }))))
    node_prop_names <- node_prop_names[!node_prop_names %in% c("id", "category")]

    # res$nodes sample:
    # List of 2
    # $ 123793 :List of 3
    # ..$ id        : chr "123793"
    # ..$ labels    :List of 1
    # .. ..$ : chr "biolink:PhenotypicFeature"
    # ..$ properties:List of 9
    # .. ..$ iri           : chr "http://purl.obolibrary.org/obo/UPHENO_0080059"
    # .. ..$ synonym       :List of 1
    # .. .. ..$ : chr "small cell"
    # .. ..$ name          : chr "decreased size of the cell"
    # .. ..$ namespace     : chr "UPHENO"
    # .. ..$ description   : chr "A reduction in the size of the cell."
    # .. ..$ provided_by   : chr "phenio_nodes"
    # .. ..$ id            : chr "UPHENO:0080059"
    # .. ..$ category      :List of 1
    # .. .. ..$ : chr "biolink:PhenotypicFeature"
    # .. ..$ exact_synonyms: chr "small cell"
    # $ 1210966:List of 3
    # ..$ id        : chr "1210966"
    # ..$ labels    :List of 1
    # .. ..$ : chr "biolink:Cell"
    # ..$ properties:List of 9
    # .. ..$ iri        : chr "http://purl.obolibrary.org/obo/CL_0000000"
    # .. ..$ xref       :List of 1
    # .. .. ..$ : chr [1:10] "CALOHA:TS-2035" "FBbt:00007002" "FMA:68646" "GO:0005623" ...
    # .. ..$ name       : chr "cell"
    # .. ..$ namespace  : chr "CL"
    # .. ..$ description: chr "A material entity of anatomical origin (part of or deriving from an organism) that has as its parts a maximally"| __truncated__
    # .. ..$ provided_by: chr "phenio_nodes"
    # .. ..$ id         : chr "CL:0000000"
    # .. ..$ category   :List of 1
    # .. .. ..$ : chr "biolink:Cell"
    # .. ..$ subsets    : chr "_upper_level|cellxgene_subset"
    #

    # this loop pulls together each property (which may be missing for some entries hence the NULL -> NA mapping)
    # as a dataframe column. However, we don't know whether what comes out should be a vector or list column
    # this previously used sapply, replaced with lapply and check for vector-able conversion
    for (prop_name in node_prop_names) {
        temp <- lapply(res$nodes, function(node) {
            prop_value <- node$properties[[prop_name]]
            if (is.null(prop_value)) {
                return(NA)
            } else {
                return(prop_value)
            }
        })

        # the resulting list can be represented as a vector if all elements are not lists and length 1
        checks <- unlist(lapply(temp, function(el) {
            !is.list(el) & length(el) == 1
        }))

        if (all(checks)) {
            nodes_df[[prop_name]] <- unlist(temp)
        } else {
            nodes_df[[prop_name]] <- temp
        }
    }

    nodes_df <- unname_cols(nodes_df)

    if (is.null(res$relationships[[1]])) {
        g <- tbl_kgx(nodes_df, attach_engine = engine)
        return(g)
    }

    ## edge info
    edge_subjects <- unlist(lapply(res$relationships, function(relationship) {
        relationship$properties$subject
    }))

    edge_predicates <- unlist(lapply(res$relationships, function(relationship) {
        relationship$properties$predicate
    }))

    edge_objects <- unlist(lapply(res$relationships, function(relationship) {
        relationship$properties$object
    }))

    edges_df <- data.frame(
        subject = edge_subjects,
        predicate = edge_predicates,
        object = edge_objects
    )

    # add all other edge properties as columns
    edge_prop_names <- unname(unique(unlist(lapply(res$relationships, function(edge) {
        names(edge$properties)
    }))))
    edge_prop_names <- edge_prop_names[!edge_prop_names %in% c("subject", "predicate", "object")]

    for (prop_name in edge_prop_names) {
        # see above RE lapply
        temp <- lapply(res$relationships, function(edge) {
            prop_value <- edge$properties[[prop_name]]
            if (is.null(prop_value)) {
                return(NA)
            } else {
                return(prop_value)
            }
        })

        # the resulting list can be represented as a vector if all elements are not lists and length 1
        checks <- unlist(lapply(temp, function(el) {
            !is.list(el) & length(el) == 1
        }))

        if (all(checks)) {
            edges_df[[prop_name]] <- unlist(temp)
        } else {
            edges_df[[prop_name]] <- temp
        }
    }

    # set from and to info for graph
    edges_df$from <- edge_subjects
    edges_df$to <- edge_objects

    edges_df <- unname_cols(edges_df)

    g <- tbl_kgx(nodes_df, edges_df, attach_engine = engine)
    attr(g, "relationship_ids") <- relationship_ids_contained
    return(g)
}

internal_cypher_query <- function(engine, query, parameters = NULL, ...) { #
    if (length(query) == 1) {
        res <- neo2R::cypher(engine$graph_conn, query = query, parameters = parameters, result = "graph")
        # NB: this will be NULL if there are no matches.
        return(neo2r_to_kgx(res, engine = engine))
    } else {
        res <- neo2R::multicypher(engine$graph_conn, queries = query, parameters = parameters, result = "graph")
        graphs <- lapply(res, neo2r_to_kgx, engine = engine)
        g <- tbl_kgx(nodes = data.frame())
        for (g2 in graphs) {
            suppressMessages(g <- tidygraph::graph_join(g, g2), classes = "message") # suppress joining info
        }
        return(g)
    }
}

# internal_cypher_query_memoised <- memoise::memoise(internal_cypher_query)

########### Public functions ###########

#' @export
#' @importFrom neo2R cypher
#' @importFrom neo2R multicypher
#' @importFrom tibble tibble
#' @importFrom tidygraph graph_join
cypher_query.neo4j_engine <- function(engine, query, parameters = NULL, ...) { #

    if (!is.null(engine$cache)) {
        # ok, this is a bit wonky
        # the engine stores its cache
        # we create a memoized internal function using that cache
        # and then we call the function
        # BUT, the engine itself needs to be sent to the function,
        # and if its cache keeps changing it wont memoize properly
        # so we create a copy of the engine without a cache and use that
        engine_copy <- engine
        engine_copy$cache <- NULL

        internal <- memoise::memoise(internal_cypher_query, cache = engine$cache)
        res <- internal(engine_copy, query, parameters, ...)

        # before we return, we reset the cache of the engine attached to the graph.
        res$engine$cache <- engine$cache
        return(res)
    } else {
        internal_cypher_query(engine, query, parameters, ...)
    }
}
