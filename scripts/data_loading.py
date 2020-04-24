from os.path import join, dirname, abspath
import pandas as pd
import geopandas as gpd

### fancy way of setting working directory
DATA_FOLDER = join(dirname(abspath(dirname(__file__))), "data")
OUTPUT_FOLDER = join(dirname(abspath(dirname(__file__))), "output")
SHAPE_FILES = ["mwi_admbnda_adm2_nso_20181016.shp", "mwi_admbnda_adm3_nso_20181016.shp"]
CURRENT_INFECTIONS = "CurrentInfectionLocation.csv"

### setting simulation parameters
ADJ_MULTIPLIER = .6
SECOND_ADJ_MULTIPLIER = .2



def go():

	adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict = create_relations()
	CI = import_current_infections()

	output = adm3_homes.copy(deep=True)
	output = output.merge(CI, how='left', left_on="ADM2", right_index=True)

	# TA_dict = build_TAs(df, adm3_to_adm2_dict, adm3_to_adm3_dict)
	print("in go\nthis is output\n", output)
	return output, adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict


def create_relations():

	### adm2 file
	adm2 = gpd.read_file(join(DATA_FOLDER, SHAPE_FILES[0]))  ## load file
	adm2 = adm2[["ADM2_EN", "geometry"]].rename({"ADM2_EN": "ADM2"}, axis=1) ## subset and rename cols

	### adm3 file
	adm3 = gpd.read_file(join(DATA_FOLDER, SHAPE_FILES[1]))
	adm3 = adm3[["ADM3_EN", "ADM2_EN", "geometry"]].rename({"ADM3_EN": "ADM3", \
		"ADM2_EN": "ADM2"}, axis=1)
	adm3_homes = adm3[["ADM2", "ADM3"]]

	### connect adm3s to 2s
	tmp = gpd.sjoin(adm3, adm2, how='left', op='intersects') ## spacial join
	adm3_to_adm2 = tmp[tmp["ADM2_left"] != tmp["ADM2_right"]] ## filter out matches with home adm
	adm3_to_adm2_dict = df_to_dict(adm3_to_adm2[["ADM3", "ADM2_right"]])

	### connect adjacent adm3s
	tmp = gpd.sjoin(adm3, adm3, how="left", op='intersects')
	# print(tmp.columns)
	adm3_to_adm3 = tmp.loc[tmp["ADM3_left"] != tmp["ADM3_right"], ["ADM3_left", "ADM3_right"]]
	adm3_to_adm3_dict = df_to_dict(adm3_to_adm3[["ADM3_left", "ADM3_right"]])

	return adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict


def import_current_infections():

	tmp = pd.read_csv(join(DATA_FOLDER, CURRENT_INFECTIONS))
	CI = tmp[["ADM2_EN", "Current Infections"]].rename({"ADM2_EN": "ADM2"}, axis=1)

	return CI.set_index("ADM2")


def df_to_dict(df):
	'''
	Creates a dictionary from a df with 2 columns
	First column becomes key, second becomes value
	'''

	d = {}

	for k, v in df.itertuples(index=False, name=None):
		d[k] = d.get(k, []) + [v]

	return d