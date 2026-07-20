# render.R — run from project root: Rscript render.R
# Renders deliverables/09_final_report_v2.Rmd as PDF and HTML,
# copies outputs to deliverables/, then cleans temp files from root.
# Must run from project root so pandoc uses relative --extract-media paths.

src <- "deliverables/09_final_report_v2.Rmd"

# Copy source to root (required for relative pandoc paths to work on Windows)
file.copy(src, "09_final_report_v2.Rmd", overwrite = TRUE)

rmarkdown::render("09_final_report_v2.Rmd", "pdf_document",  "09_final_report_v2.pdf")
file.copy("09_final_report_v2.pdf",  "deliverables/09_final_report_v2.pdf",  overwrite = TRUE)

rmarkdown::render("09_final_report_v2.Rmd", "html_document", "09_final_report_v2.html")
file.copy("09_final_report_v2.html", "deliverables/09_final_report_v2.html", overwrite = TRUE)

# Clean up root (intermediate files; rmarkdown clean=TRUE handles .knit.md and fig cache)
unlink(c("09_final_report_v2.Rmd", "09_final_report_v2.pdf", "09_final_report_v2.html",
         "09_final_report_v2.tex", "09_final_report_v2.log"), force = TRUE)
unlink("09_final_report_v2_files", recursive = TRUE, force = TRUE)

cat("Done — deliverables/ updated.\n")
