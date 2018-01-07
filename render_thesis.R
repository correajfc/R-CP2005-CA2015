
bookdown::render_book('index.Rmd', 'bookdown::gitbook')
#bookdown::preview_chapter("procesa_analisis.Rmd",'bookdown::gitbook')

bookdown::render_book("index.Rmd", "bookdown::pdf_book")

bookdown::render_book("index.Rmd", "bookdown::word_document2")