@echo off
rem Shortcut to render site (into "_book" folder) without starting RStudio

"C:\Program Files\R\R-4.2.0\bin\RScript" -e "rmarkdown::render_site(output_format = 'bookdown::bs4_book', encoding = 'UTF-8')"