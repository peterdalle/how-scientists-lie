knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = FALSE,
  warning = FALSE,
  message = FALSE,
  echo = FALSE,
  dpi = 300,
  cache.lazy = FALSE,
  tidy = "styler",
  out.width = "90%",
  fig.align = "center",
  fig.width = 5,
  fig.height = 7
)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))


# Include videos in RMarkdown chunks (don't forget results="asis")
include_video <- function(src, width="", height="", type="video/mp4", attributes="controls",
                          class="video-player mt-3", prefix="Video: ", caption="", default="") {
  if (knitr::is_html_output()) {
    print(glue::glue('
    <div class="video">
      <video width="{width}" height="{height}" class="{class}" {attributes}>
        <source src="{src}" type="{type}">
      </video>
      <p class="caption">{prefix}{caption}</p>
    </div>'))
  } else {
    print(default)
  }
}
