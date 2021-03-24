# codata app
## run app on R studio host
https://magnusmunch.shinyapps.io/codata-app/

## run app locally in R
In R:  
install.packages(c("shiny", "shinyBS", "openxlsx", "survival", "pROC",
                   "devtools", "glmnet", "gren"))  
install_github("Mirrelijn/ecpc/Rpackage")  
runGitHub("codata-app", "magnusmunch", subdir="app")

## Vignette
* compile docs/vignette.rnw for the vignette (e.g., in R studio using knitr)
* citation style is in docs/author_short3.bst file
* bib references are in docs/refs.bib
