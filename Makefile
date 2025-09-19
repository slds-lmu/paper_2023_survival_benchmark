help:
	@echo "Available targets:"
	@echo "  results-paper  - Fun results-paper-plots.R for figures & tables used in publication"
	@echo "  results        - Run process-results.R to start the (long and memory intensive!) task of aggregating batchtools result objects to results/production/"
	@echo "  site           - Render results ./site/ with quarto"
	@echo "  predictions    - Run store-predictions.R to store the outer resampling learner prediction objects alongside other results"
	@echo "-------------------------------------------------------------------------"
	@echo "  upload-site    - Private use: Upload result site to S3 bucket"
	@echo "  upload-results - Private use: Upload result data to S3 bucket"

.PHONY: results-paper
results-paper:
	Rscript produce-paper-plots.R

.PHONY: results
results:
	Rscript process-results.R

.PHONY: predictions
predictions:
	Rscript store-predictions.R

.PHONY: site
site:
	quarto render site

.PHONY: upload-results
upload-results: results/index.html
	rclone sync  -P --header "Cache-Control: max-age=120,public" --exclude "*.csv" results/production/ aws_jemsu:jemsu/survival_benchmark/results/

.PHONY: upload-site
upload-site:
	rclone copy -P --header "Cache-Control: max-age=120,public"  site/_site/ aws_jemsu:jemsu/survival_benchmark/

results/index.html:
	cd results/production && tree -D -h -T "Survival Benchmark Results" -I "*index.html*|tuning_archives" -H . -o index.html
