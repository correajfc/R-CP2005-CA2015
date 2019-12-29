
bookdown::render_book('index.Rmd', 'bookdown::gitbook')
beepr::beep(7)

bookdown::render_book("tesis_pdf.Rmd", "bookdown::pdf_book")
beepr::beep(8)

