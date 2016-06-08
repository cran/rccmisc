# Vignettes that depend on internet access have been precompiled:

# knitr::knit("vignettes/dependencies.Rmd.orig", "vignettes/dependencies.Rmd")
knitr::knit("vignettes/rccmisc.Rmd.orig", "vignettes/rccmisc.Rmd")
rmarkdown::render("vignettes/rccmisc.Rmd", output_file = "rccmisc.html")
devtools::build_vignettes()
