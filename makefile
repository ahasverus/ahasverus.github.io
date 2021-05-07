all: index.html

index.html: data/* R/* layout/css/* layout/js/* layout/img/* files/pdfs/* files/bibs/*
	Rscript "R/render_site.R"
