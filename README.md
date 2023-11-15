# ST558 Project 3

### Purpose:

The purpose of this project is to automate R Markdown reports involving creating predictive models on a subset of diabetes data. For each report we divide the data into an education band, attempt several predictive models to predict a binary outcome based on that particular education group and then choose the best model. 

### R Packages

The R packages used in this project are

* [tidyverse](https://www.tidyverse.org/)
* [summarytools](https://cran.r-project.org/package=summarytools/vignettes/introduction.html)
* [caret](https://cran.r-project.org/web/packages/caret/vignettes/caret.html)
* [rattle](https://cran.r-project.org/web/packages/rattle/index.html)

### Automation: 

To automate the reports a single Rmarkdown document was created with 2 parameters in the yaml header education which takes numeric value representing the level of education variable, and title which accepts a character string for the title of the document to be rendered. 

Separate .md files were then rendered from the single .Rmd file using the following code: 

```r
rmarkdown::render("Project 3 Work.Rmd", params = list(education = 1, title = "Never attended school or only kindergarten" ), output_file = "Grades0-k")
rmarkdown::render("Project 3 Work.Rmd", params = list(education = 2, title = "Grades 1 through 8 (Elementary)" ), output_file = "Grades1_8")
rmarkdown::render("Project 3 Work.Rmd", params = list(education = 3, title = "Grades 9 through 11 (Some high school)" ), output_file = "Grades9_11")
rmarkdown::render("Project 3 Work.Rmd", params = list(education = 4, title = "Grade 12 or GED (High school graduate)" ), output_file = "Grades12_GED")
rmarkdown::render("Project 3 Work.Rmd", params = list(education = 5, title = "College 1 year to 3 years (Some college or technical school)" ), output_file = "College1_3")
rmarkdown::render("Project 3 Work.Rmd", params = list(education = 6, title = "College 4 years or more (College graduate)" ), output_file = "College4")
```

### Automated Report Links

You can view each of the automated reports by clicking the links below

[Never attended school or only kindergarten](Grades0-k.md)
[Grades 1 through 8 (Elementary)](Grades1-8.md)
[Grades 9 through 11 (Some high school)](Grades9-11.md)
[Grade 12 or GED (High school graduate)](Grades12-GED.md)
[College 1 year to 3 years (Some College)](College1-3.md)
[College 4 years or more (College graduate)](College4.md)