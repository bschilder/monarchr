skip_if_lite <- function() {
    testthat::skip_if(
        tolower(Sys.getenv("TEST_LITE", "false")) == "true",
        "Skipping slow integration test in lite mode"
    )
}
