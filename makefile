all: index.html

index.html: data/* R/* layout/css/* layout/js/* layout/img/* files/pdfs/* files/bibs/* build_site.R
	Rscript "build_site.R"
