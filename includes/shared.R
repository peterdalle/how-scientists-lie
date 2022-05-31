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
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(ggplot2))

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
# built book is used for the word count because it uses the merged file.
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

# Primary book color (used for line charts, bar charts, plotting)
primary_color <- function() {
  "#52A0C2"
}

# Show pull quote, but only for HTML not LaTeX/PDF
pull_quote <- function(quote) {
  if (is_starting_or_ending_with_quotes(quote)) {
    stop("Don't start or end with quotes within pull_quote(), we'll add these ourselves!")
  }
  if (knitr::is_html_output()) {
    print(glue::glue('
    <div class="pull-quote"><blockquote>{quote}</blockquote></div>'))
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

# prints a list of resources from a data frame
print_resource_links <- function(resources) {
  for (i in seq.int(NROW(resources))) {
      resource_link(title=resources$title[i], image=resources$image[i],
                    url=resources$url[i], description=resources$description[i])
  }
}

# Adds a image/link resource
resource_link <- function(title, image, url, description) {
  if (knitr::is_html_output()) {
    print(glue::glue('<div class="card mb-4"><div class="row no-gutters"><div class="col-sm-3"><a href="{url}"><img class="card-img" src="{image}" alt="{title}"></a></div><div class="col-sm-9"><div class="card-body"><h3 class="card-title"><a href="{url}">{title}</a></h3><p class="card-text">{description}</p></div></div></div></div>'))
  }
}

# Plots ggplot2 as an interactive (HTML) or static (PDF) figure
plot_interactive_figure <- function(fig, ...) {
  if (knitr::is_html_output()) {
   plotly::ggplotly(fig, ...) |>
     plotly::config(displayModeBar = FALSE) |>
     plotly::layout(font = list(family = "Arial"),
                    hoverlabel = list(bgcolor = "white"))
  } else {
    fig
  }
}

#' Generates random draws from multivariate distribution
#'
#' @param n number of draws.
#' @param p number of variables.
#' @param u mean of each variable.
#' @param s standard deviation of each variable.
#' @param cor_mat correlation matrix.
#'
#' @return
#' @export
#'
#' @examples
mvrnorm <- function(n, p, u, s, cor_mat) {
  # Modified from Jim Grange
  # https://gist.github.com/JimGrange/6e9ab3338f16eaf731bca9cce1e2e5e2
  Z <- matrix(rnorm(n * p), p, n)
  t(u + s * t(chol(cor_mat)) %*% Z)
}

#' @param mean vector with means.
#' @param sd vector with standard deviations.
#' @param n_simulations number of simulations.
#' @param r corelation coefficient.
#' @param n vector of sample sizes to simulate.
#'
#' @return list with all data and only the significant data.
#' @export
#'
#' @examples
simulate_phacking <- function(mean = c(100, 600), sd = c(20, 89), n_simulations = 100, r = 0.21,
                              n = c(10, 30, 50, 100, 150, 500, 1000), sig.level = 0.05) {
  # Modified from Jim Grange
  # https://gist.github.com/JimGrange/6e9ab3338f16eaf731bca9cce1e2e5e2
  cor_mat <- matrix(c(1, r, r, 1), nrow = 2, ncol = 2, byrow = TRUE)
  all_data <- matrix(nrow = n_simulations, ncol = length(n))
  sig_data <- all_data
  colnames(all_data) <- n
  colnames(sig_data) <- n

  # Perform simulation.
  for(i in seq.int(length(n))) {
    for(j in seq.int(n_simulations)) {
      data <- mvrnorm(n[i], p=2, u=mean, s=sd, cor_mat=cor_mat)
      correlations <- cor.test(data[, 1], data[, 2], method="pearson")
      estimate <- as.numeric(round(abs(correlations$estimate), 3))
      all_data[j, i] <- estimate
      if(correlations$p.value < sig.level){
        # Save effect size when the p-value is significant
        sig_data[j, i] <- estimate
      }
    }
  }
  list(all_data = as.data.frame(all_data),
       sig_data = as.data.frame(sig_data))
}
