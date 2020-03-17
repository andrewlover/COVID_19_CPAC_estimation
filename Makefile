
R := Rscript --vanilla
PYTHON := python3 -W ignore

CPACanalysis:
	mkdir -p analysisData && mkdir -p figs && \
	$(R) createModelPredictions.R && \
	$(PYTHON) plotTrajectory.py && \
	echo 'analysis complete'
