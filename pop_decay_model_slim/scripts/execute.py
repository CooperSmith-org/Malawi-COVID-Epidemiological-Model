import pandas as pd
import geopandas as gpd
import numpy as np
import sys
import datetime
import drop_to_dropbox
from os.path import join, abspath, dirname, exists

import pull_current_infections
import pop_decay

SHAPE_PATH = "../static/shape/mwi_admbnda_adm3_nso_20181016.shp"
EXCLUSIONS = {"ADM3_PCODE": (["MW20115", "MW30399", "MW30904", "MW30299",
			"MW20511", "MW30199", "MW31009", "MW31305",
			"MW30807", "MW31110", "MW20207", "MW10106",
			"MW10206", "MW10410", "MW10511", "MW10411"])}
BUNDLES = {"TO_BUNDLE": "ADM2_PCODE",
			"REGIONS": (["MW206", "MW305", "MW303", "MW105"]),
			"SUBREGION": "ADM3_PCODE"}
CONSOLIDATIONS = {"MW315": "MW305",
				"MW210": "MW206",
				"MW107": "MW105",
				"MW314": "MW303"}
ADJ_MULTIPLIERS = {1: .6, 2: .2}
DEGREE = 2


def main():
	"""
	executes population decay model
	"""

	### load shape file
	mw = gpd.read_file(SHAPE_PATH)

	### get adjacency matrix
	adj_dict = pop_decay.create_adj_dict(mw, EXCLUSIONS, BUNDLES, CONSOLIDATIONS)
	adj_matrix = pop_decay.create_adj_matrix(adj_dict, DEGREE)
	adj_matrix = pop_decay.convert_adj_matrix_to_multipliers(adj_matrix, ADJ_MULTIPLIERS)

	### get current infections
	CI = pull_current_infections.main(mw, BUNDLES)
	CI = CI[~CI.index.duplicated()]
	CI_adj = CI.reindex(adj_matrix.index).fillna(0).sort_index()

	### carry out analysis
	product = pop_decay.calculate_decay(adj_matrix, CI_adj, ADJ_MULTIPLIERS)

	### get results
	CI = CI.replace(0, np.nan).combine_first(product)

	### export results to drop box
	drop_to_dropbox.upload_file(CI)


if __name__ == '__main__':
	main()