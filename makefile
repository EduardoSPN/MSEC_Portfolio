hw6.html:	hw6.Rmd	github_data/Weather_data.Rds	data/bike_df2.Rds	data/cbs_test.csv	data/dist_df.Rds	data/inventory.Rds
	Rscript -e "library(rmarkdown); render('hw6.Rmd')"
 
.PHONY: clean_html clean_data
clean_html:
	rm hw6.html
 
clean_data:
	rm -rf data/