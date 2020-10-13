.onLoad <- function(lib, pkg) {
  generate_hpop_populations <<- memoise::memoise(generate_hpop_populations)
}
