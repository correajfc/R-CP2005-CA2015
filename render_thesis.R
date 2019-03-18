
bookdown::render_book('index.Rmd', 'bookdown::gitbook')
beepr::beep(8)
#bookdown::preview_chapter("procesa_analisis.Rmd",'bookdown::gitbook')


bookdown::render_book("index.Rmd", "bookdown::pdf_book")


nbookdown::render_book(c("index.Rmd", 
                      "abstract.Rmd", 
                      "intro.Rmd",
                      "revision_literatura.Rmd",
                      #"metodologia.Rmd",
                      # "resultados.Rmd",
                      #  "discusion.Rmd",
                      #"conclusiones.Rmd",
                      "referencias.Rmd"), "bookdown::pdf_book", preview = TRUE)

#bookdown::render_book("index.Rmd", "bookdown::word_document2")
