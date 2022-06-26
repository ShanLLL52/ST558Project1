# Code for create README.md
rmarkdown::render("/Users/sl666/Desktop/ST558/ST558Project1/ST558Project1.Rmd", 
                  output_format = "github_document",
                  output_file = "README.md",
                  output_options = list(
                    html_preview = FALSE))