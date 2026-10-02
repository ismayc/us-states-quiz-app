# Builds the browser icons and the link-preview image for the shinylive site
# from the app's own state shapes. The SVG files are the sources; the PNGs are
# rendered from them with magick.
#
# Re-render from the repo root with:
#   Rscript site/make_assets.R
# and commit the updated files in site/.

library(sf)

states <- readRDS("states_enriched2023.rds")
out <- "site"

# The contiguous states and DC, simplified, in a US Albers projection
lower48 <- states[!states$state_name %in% c("Alaska", "Hawaii"), ] |>
  st_transform(5070) |>
  st_simplify(dTolerance = 6000, preserveTopology = TRUE)
country <- lower48 |> st_union() |> st_simplify(dTolerance = 15000)

# Turn sf geometry into an SVG path, fitted into a box at (x0, y0) of size w x h
to_path <- function(geom, bbox, x0, y0, w, h) {
  sx <- w / (bbox["xmax"] - bbox["xmin"])
  sy <- h / (bbox["ymax"] - bbox["ymin"])
  s <- min(sx, sy)
  dx <- x0 + (w - s * (bbox["xmax"] - bbox["xmin"])) / 2
  dy <- y0 + (h - s * (bbox["ymax"] - bbox["ymin"])) / 2
  rings <- st_coordinates(st_cast(st_geometry(geom), "MULTIPOLYGON"))
  key <- interaction(rings[, "L1"], rings[, "L2"], rings[, "L3"], drop = TRUE)
  parts <- lapply(split(as.data.frame(rings), key), function(r) {
    x <- dx + s * (r$X - bbox["xmin"])
    y <- dy + s * (bbox["ymax"] - r$Y)
    paste0("M", paste(sprintf("%.1f,%.1f", x, y), collapse = "L"), "Z")
  })
  paste(parts, collapse = "")
}

bbox <- st_bbox(country)

navy <- "#0A3161"
red <- "#B31942"

# ---- favicon.svg (64 x 64) -------------------------------------------------
favicon <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64">
  <rect width="64" height="64" rx="12" fill="#ffffff"/>
  <path d="%s" fill="#000000"/>
  <rect x="8" y="50" width="24" height="5" fill="%s"/>
  <rect x="32" y="50" width="24" height="5" fill="%s"/>
</svg>
',
  to_path(country, bbox, 4, 12, 56, 34), navy, red
)
writeLines(favicon, file.path(out, "favicon.svg"))

# ---- og-image.svg (1200 x 630) ---------------------------------------------
state_paths <- vapply(
  seq_len(nrow(lower48)),
  function(i) to_path(lower48[i, ], bbox, 40, 150, 560, 350),
  character(1)
)
og <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" width="1200" height="630" viewBox="0 0 1200 630">
  <rect width="1200" height="630" fill="#f4f4f2"/>
  %s
  <g font-family="Helvetica, Arial, sans-serif">
    <text x="650" y="230" font-size="64" font-weight="700" fill="#111111">US State/District</text>
    <text x="650" y="305" font-size="64" font-weight="700" fill="#111111">Shape Quiz</text>
    <text x="650" y="375" font-size="30" fill="#333333">Name all 50 states and DC from their</text>
    <text x="650" y="418" font-size="30" fill="#333333">outlines, with optional capitals and</text>
    <text x="650" y="461" font-size="30" fill="#333333">largest cities.</text>
    <text x="650" y="525" font-size="28" fill="#555555">Created by Chester Ismay</text>
  </g>
  <rect x="650" y="550" width="250" height="14" fill="%s"/>
  <rect x="900" y="550" width="250" height="14" fill="%s"/>
</svg>
',
  paste(sprintf('<path d="%s" fill="#111111" stroke="#ffffff" stroke-width="1.5"/>',
                state_paths), collapse = "\n  "),
  navy, red
)
writeLines(og, file.path(out, "og-image.svg"))

# ---- PNG renders ------------------------------------------------------------
# magick's built-in SVG reader sizes by density (96 dpi = 1 SVG unit per pixel),
# so the density is scaled to hit the target width exactly
render <- function(svg, png, width, svg_width, flatten = FALSE) {
  img <- magick::image_read(file.path(out, svg), density = 96 * width / svg_width)
  if (flatten) img <- magick::image_background(img, "#ffffff", flatten = TRUE)
  magick::image_write(img, file.path(out, png), format = "png")
}
render("favicon.svg", "favicon-32.png", 32, 64)
# iOS fills transparent corners black, so the touch icon is flattened onto white
render("favicon.svg", "apple-touch-icon.png", 180, 64, flatten = TRUE)
render("og-image.svg", "og-image.png", 1200, 1200)

message("Wrote ", paste(list.files(out, "\\.(svg|png)$"), collapse = ", "))
