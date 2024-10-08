library(testthat)
library(assertthat)


test_that("fetch_nodes neo4j works with basid id query", {
    #testthat::skip("temporary skip")

    e <- monarch_engine()

    # fetch_nodes(id %in% c("MONDO:0007525", "HGNC:4635")) should result in an error
    # do so silently in the logs...
    g <- fetch_nodes(e, query_ids = c("MONDO:0007525", "HGNC:4635"))

    nodes_df <- g %>% activate(nodes) %>% as.data.frame()
    # there should be an id column with 2 entries: MONDO:0007525 and HGNC:4635,
    # but we can't gaurantee the order
    expect_contains(2 + -1:2, nrow(nodes_df))
    expect_true(all(nodes_df$id %in% c("MONDO:0007525", "HGNC:4635")))

    # there should be no edges
    edges_df <- g %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(edges_df), 0)
})

test_that("fetch_nodes neo4j works with complex query syntax", {
    e <- monarch_engine()
    g <- e %>% fetch_nodes(id == "MONDO:0007525")

    nodes_df <- g %>% activate(nodes) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 1)
    expect_equal(nodes_df$id, "MONDO:0007525")

    # check to see that we can chain the fetch_nodes function with other functions
    g <- e %>%
      fetch_nodes(id == "MONDO:0007525") %>%
      expand(categories = "biolink:Gene")

    # this result should have 3 nodes and 3 edges
    nodes_df <- g %>% activate(nodes) %>% as.data.frame()
    expect_contains(3 + -2:2, nrow(nodes_df))

    edges_df <- g %>% activate(edges) %>% as.data.frame()
    expect_contains(3 + -2:2, nrow(edges_df))
})

test_that("fetch_nodes limit works with neo4j_engine", {
	e <- monarch_engine()
	expect_warning(
	g <- e %>% fetch_nodes(in_taxon_label == "Homo sapiens", limit = 10))

	nodes_df <- g %>% activate(nodes) %>% as.data.frame()
	expect_contains(10 + -2:2, nrow(nodes_df))

	e <- monarch_engine()
	expect_warning(
	g <- e %>% fetch_nodes(in_taxon_label == "Homo sapiens", limit = 5))

	nodes_df <- g %>% activate(nodes) %>% as.data.frame()
	expect_contains(5 + -1:2, nrow(nodes_df))
})
