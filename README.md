# How Scientists Lie – Book Repository

This is the work-in-progress repo for the book [**How Scientists Lie**](https://howscientistslie.com/) by [Peter M. Dahlgren](https://peterdahlgren.com/).

The book is built using [bookdown](https://bookdown.org/).

## Compile website

Compile the book by opening `index.rmd` in RStudio and press `Build Book`. This will create files in a new folder called `_book`.

An alternative way is to run `build_site.cmd` (however, check the path to the `RScript` executable).

## Directories

- `includes` contains misc files that are included when compiling the book.
- `data` contains data used in the book.
- `images` contains images, graphics, and figures.
- `videos` contains video clips used on the website.

Edit the book contents using the RMarkdown files in the root folder.

## Files in the `data` folder

File | Description
--------- | ---------------------------
`p-hacking-false-positives.csv` | Extracted data from figure 11 in [Big Little Lies: A Compendium and Simulation of p-Hacking Strategies](...)
`prevalence_of_qrps.csv` | Average prevalence estimate of QRPs (questionable research practices) from different references
`prevalence_of_term_qrp.yml` | Occurence of the term "questionable research practices" in academic databases
`resources.csv` | Website links and screenshots used for the resources chapter

## License

The book licensed under a [Creative Commons license](LICENSE). 

By participating in this project you agree to abide by its terms.
