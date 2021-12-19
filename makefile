all: index.html

index.html: data/* R/* layout/css/* layout/js/* layout/img/* files/pdfs/* files/bibs/*
	Rscript "build_site.R"
