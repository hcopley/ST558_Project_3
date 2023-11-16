
rmarkdown::render("Project 3 Work.Rmd", output_format = 'github_document', params = list(education = 1, title = "Never attended school or only kindergarten" ), output_file = "Grades0tok")
rmarkdown::render("Project 3 Work.Rmd", output_format = 'github_document', params = list(education = 2, title = "Grades 1 through 8 (Elementary)" ), output_file = "Grades1to8")
rmarkdown::render("Project 3 Work.Rmd", output_format = 'github_document', params = list(education = 3, title = "Grades 9 through 11 (Some high school)" ), output_file = "Grades9to11")
rmarkdown::render("Project 3 Work.Rmd", output_format = 'github_document', params = list(education = 4, title = "Grade 12 or GED (High school graduate)" ), output_file = "Grades12toGED")
rmarkdown::render("Project 3 Work.Rmd", output_format = 'github_document', params = list(education = 5, title = "College 1 year to 3 years (Some college or technical school)" ), output_file = "College1to3")
rmarkdown::render("Project 3 Work.Rmd", output_format = 'github_document', params = list(education = 6, title = "College 4 years or more (College graduate)" ), output_file = "College4")

#source('Source.R')

#remove all html files (keeping only the .md files)
list.files() %>%
    keep(str_detect(., '.html')) %>% 
    discard(str_detect(., '_includes')) %>%
    map(file.remove)


