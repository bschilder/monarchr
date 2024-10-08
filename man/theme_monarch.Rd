% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_monarch.R
\name{theme_monarch}
\alias{theme_monarch}
\title{Monarch Initiative plot theme}
\usage{
theme_monarch(
  palettes = monarch_palettes(),
  layer_args = list(theme_bw = list(), theme = list(axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()),
    scale_edge_color_manual = list(values = palettes$edges$discrete, na.value = "grey"),
    scale_color_manual = list(values = palettes$nodes$discrete, na.value = "grey"),
    scale_shape_manual = list(values = palettes$nodes$shape, 
     na.value = 19),
    scale_edge_fill_manual = list(values = palettes$edges$discrete, na.value = "grey",
    guide = "none"))
)
}
\arguments{
\item{palettes}{A named list of discrete/continuous palettes for nodes/edges
Default is \code{monarch_palettes}.}

\item{layer_args}{A named list of arguments for each \link{ggplot} layer.}
}
\value{
\pkg{ggplot2}/\pkg{ggraph} object.
}
\description{
A \pkg{ggplot2}/\pkg{ggraph} theme inspired by the
\href{https://monarchinitiative.org/}{Monarch Initative}.
}
\examples{
filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
g <- file_engine(filename) |>
          fetch_nodes(query_ids = "MONDO:0007525") |>
          expand(predicates = "biolink:has_phenotype",
                 categories = "biolink:PhenotypicFeature")|>
          expand(categories = "biolink:Gene")
plot(g, layout="kk") + theme_monarch()
}
