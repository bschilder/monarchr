devtools::load_all()

devtools::document()

devtools::clean_vignettes()
devtools::build_vignettes()
devtools::build_site()
pkgdown::build_articles()

devtools::test()

devtools::check()

devtools::build()

devtools::install()
# hmm

Sys.setenv(PATH = paste("/opt/homebrew/bin", Sys.getenv("PATH"), sep = ":"))
# Optional fallback to RStudio-bundled pandoc:
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")
rmarkdown::find_pandoc()
devtools::check()
