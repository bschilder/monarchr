# monarchr 0.2.2

## New features

* README:
	- Create to README.Rmd to enable autofilling. 
	- Add badges
	- Add link to docs site
* DESCRIPTION:
	- Add URL/BugReports fields
* vignettes
	- Change Basics.Rmd --> monarchr.Rmd to match conventions.
	- Name chunks in vignette
	- Add session info to end of vignette

# monarchr 0.2.1

* Add rworkflows GHA for automated checking, site building, and Docker container creation.
* Update *.Rbuildignore*.
* Add NEWS file.
* Add Title, Description, and Authors to *DESCRIPTION*.
	- Hope you don't mind me adding our names here @oneilsh, I anticipate making lots of contributions!
* Add `tidyr` Import to *DESCRIPTION*.
* Add vignettes header info.
* Fix `edges` docs by adding title.
* Add `VignetteBuilder: knitr` to *DESCRIPTION*.
* Fix `cypher_query` example by adding `ids`
* Fix `cypher_query_df` example by adding `ids`
* Export `cypher_query_df`
* Get `monarchR` closer to passing rcmdcheck (methods for edges/nodes not yet being recognized)
