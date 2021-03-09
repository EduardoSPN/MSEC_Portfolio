hw5.html: hw5.Rmd data/wawa/wawa.rds data/sheetz/sheetz.rds
	Rscript -e "library(rmarkdown); render('hw5.Rmd')"

data/wawa/wawa.rds: parse_wawa.R data/wawa/wawa_*.rds
	Rscript parse_wawa.R
	
data/wawa/wawa_*.rds: get_wawa.R
	mkdir -p data/wawa
	Rscript get_wawa.R

data/sheetz/sheetz.rds: parse_sheetz.R data/sheetz/sheetz_*.rds
	Rscript parse_sheetz.R

data/sheetz/sheetz_*.rds: get_sheetz.R
	mkdir -p data/sheetz
	Rscript get_sheetz.R

.PHONY: clean_html clean_data clean_wawa clean_sheetz

clean_html:
	rm hw5.html
	rm -rf hw5_files/
	
clean_data:
	rm -rf data/
	
clean_wawa:
	rm -rf data/wawa/
	
clean_sheetz:
	rm -rf data/sheetz/
