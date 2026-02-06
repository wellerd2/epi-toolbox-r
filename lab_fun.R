# lab_fun.R
# Helper for plot labeling:
# Produces plotmath expressions (as strings) that italicize most binomials, while leaving
# selected labels plain (hard-coded in the function).

lab_fun <- function(x,
                    plain_labels = c("Species not identified", "Other Vibrio spp.", "Other Genus")) {
  sapply(x, function(lbl) {
    if (lbl %in% plain_labels) {
      sprintf("'%s'", lbl)
    } else {
      sp <- gsub(" ", "~", lbl)
      paste0("italic(", sp, ")")
    }
  })
}
