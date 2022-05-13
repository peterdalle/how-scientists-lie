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

# Verbose output for easier PDF debugging https://yihui.org/tinytex/r/#debugging
options(tinytex.verbose = TRUE)

# Include videos in RMarkdown chunks (don't forget results="asis")
include_video <- function(src, caption="", width="", height="", type="video/mp4", class="video-player mt-3",
                          attributes="controls", prefix="Video: ", default="") {
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

# Gives the word count of the book. Build book two times to make sure the
# built book is used for the word count.
# Note: Set "delete_merged_file: false" in _bookdown.yml.
word_count <- function(file="_main.md") {
  # Install:
  # devtools::install_github("benmarwick/wordcountaddin", type = "source", dependencies = TRUE)
  if (file.exists(file)) {
    wordcountaddin::word_count(file)
  } else {
    warning("Cannot get word count, book file '", file, "' does not exist.",
            " Try building the book again.\n",
            " Also set delete_merged_file: false in _bookdown.yml", call. = FALSE)
    0
  }
}

# Turn numbers 2500 into 2,500
format_number <- function(number, digits=0) {
  trimws(format(round(number, digits), big.mark=",", scientific=FALSE))
}

# ggplot2 book theme for plots
theme_howscientistslie <- function() {
  list(theme_classic(),
       theme(text = element_text(color="black"),
             axis.text = element_text(color="black"),
             axis.title = element_text(color="black"),
             axis.ticks.length = unit(6, "pt"),
             axis.text.x = element_text(margin = margin(t=5)),
             axis.text.y = element_text(margin = margin(r=5)),
             legend.position = "top"))
}

# Show pull quote, but only for HTML not LaTeX/PDF
pull_quote <- function(quote) {
  if (is_starting_or_ending_with_quotes(quote)) {
    stop("Don't start or end with quotes within pull_quote(), we'll add these ourselves!")
  }
  if (knitr::is_html_output()) {
    print(glue::glue('
    <div class="pull-quote">
      <blockquote>
        {quote}
      </blockquote>
    </div>'))
  }
}

is_starting_or_ending_with_quotes <- function(text) {
  text_start <- substr(text, 1, 1)
  text_end <- substr(text, nchar(text), nchar(text))
  quote_chars <- c("\"", "'")
  if (text_start %in% quote_chars | text_end  %in% quote_chars) {
    TRUE
  } else {
    FALSE
  } 
}