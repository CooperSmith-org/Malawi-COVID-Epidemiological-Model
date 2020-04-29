import data_loading
import search_TAs
import pandas as pd
from os.path import join, dirname, abspath

### to do
#### merge on readable city names
#### group cities into 1
#### upload to git

OUTPUT_FOLDER = join(dirname(abspath(dirname(__file__))), "output")
MULTIPLIER_DICT = {1: .6, 2: .2}


def go():

	degree, multiplier_dict, outfile = data_loading.get_params()

	# assert len(multiplier_dict) == degree, ("multiplier_dict length must equal degree")

	print("loading files...")
	CI, TA_Districts, TA_adj_Dist, TA_adj_TA = data_loading.load_inputs()
	TA_list = list(TA_Districts["ADM3"].unique())
	print("building connections...")
	total_cs = search_TAs.get_connections(TA_list, TA_adj_TA, CI, degree)

	print("calculating scores...")
	scores = search_TAs.calc_scores(total_cs, CI, TA_Districts, multiplier_dict)

	print("outputting to {}...".format(outfile + '.xlsx'))

	writer = pd.ExcelWriter(join(OUTPUT_FOLDER, outfile + '.xlsx'), engine='xlsxwriter')
	total_cs.to_excel(writer, sheet_name="Connections", index=False)
	scores.to_excel(writer, sheet_name="Scores", index=False)
	writer.save()

	print("Success!")
	print("See your output in the output folder")

	# return scores, total_cs

if __name__ == "__main__":

	go()