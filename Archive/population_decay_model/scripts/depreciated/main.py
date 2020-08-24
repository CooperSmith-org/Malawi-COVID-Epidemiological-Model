import data_loading
import run_model
import build_inputs
import interface
import directory_management
import pandas as pd
from os.path import join, dirname, abspath

### to do
#### merge on readable city names
#### group cities into 1
#### upload to git

OUTPUT_FOLDER = join(dirname(abspath(dirname(__file__))), "output")
COUNTRY_LIST = ["MW"]
# multiplier_dict = {1: .6, 2: .2}
# degree = 2


def go():
	"""
	executes analysis
	"""

	print("Population Decay Analysis Tool")

	# country, bundled, excluded = interface.get_initial_params(COUNTRY_LIST)
	country = "MW"
	bundled = True
	excluded = True

	adj_dict, ids = data_loading.load_adj(country, bundled, excluded)
	
	degree, multiplier_dict, outfile = interface.get_secondary_params()

	# assert len(multiplier_dict) == degree, ("multiplier_dict length must equal degree")

	print("loading files...")
	# CI_filename = input("Please enter the name of the CI file, include '.csv': ")
	CI_filename = "CurrentInfectionLocation_30April20 - Copy.csv"

	CIs = data_loading.import_CIs(CI_filename)
	
	# CI, TA_Districts, TA_adj_Dist, TA_adj_TA = data_loading.load_inputs(bundled)

	# return None
	print("building connections...")
	print("calculating scores...")
	scores, total_cs = run_model.main(ids, adj_dict, degree, multiplier_dict, CIs)
	# return scores

	print("outputting to {}...".format(outfile + '.xlsx'))
	output_folder = join(dirname(abspath(dirname(__file__))), "output", country)

	writer = pd.ExcelWriter(join(OUTPUT_FOLDER, outfile + '.xlsx'), engine='xlsxwriter')
	total_cs.to_excel(writer, sheet_name="Connections", index=False)
	scores.to_excel(writer, sheet_name="Scores", index=False)
	writer.save()

	print("Success!")
	print("See your output in the output folder")

	# return scores, total_cs

if __name__ == "__main__":

	go()