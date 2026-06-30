# render.R — run from project root: Rscript render.R
# Renders deliverables/09_final_report.Rmd as PDF and HTML,
# copies outputs to deliverables/, then cleans temp files from root.
# Must run from project root so pandoc uses relative --extract-media paths.

src <- "deliverables/09_final_report.Rmd"

# Copy source to root (required for relative pandoc paths to work on Windows)
file.copy(src, "09_final_report.Rmd", overwrite = TRUE)

rmarkdown::render("09_final_report.Rmd", "pdf_document",  "09_final_report.pdf")
file.copy("09_final_report.pdf",  "deliverables/09_final_report.pdf",  overwrite = TRUE)

rmarkdown::render("09_final_report.Rmd", "html_document", "09_final_report.html")
file.copy("09_final_report.html", "deliverables/09_final_report.html", overwrite = TRUE)

# Clean up root (intermediate files; rmarkdown clean=TRUE handles .knit.md and fig cache)
unlink(c("09_final_report.Rmd", "09_final_report.pdf", "09_final_report.html",
         "09_final_report.tex", "09_final_report.log"), force = TRUE)
unlink("09_final_report_files", recursive = TRUE, force = TRUE)

cat("Done — deliverables/ updated.\n")
