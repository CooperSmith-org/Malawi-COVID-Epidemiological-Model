from os.path import join, dirname, abspath
import pandas as pd
import geopandas as gpd
import json

### fancy way of setting working directory
DATA_FOLDER = join(dirname(abspath(dirname(__file__))), "data")
SHAPE_FILES = ["mwi_admbnda_adm2_nso_20181016.shp", "mwi_admbnda_adm3_nso_20181016.shp"]
CURRENT_INFECTIONS = "CurrentInfectionLocation.csv"

INPUTS_FOLDER = join(join(dirname(abspath(dirname(__file__))), "inputs"), "cleaned_data")


def go():
	"""
	Calls all functions that create data structures for population decay model
	"""

	adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict = create_relations()
	CI = import_current_infections()
	return CI, adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict


def load_inputs():
	"""
	Loads input files from cleaned data folder
	"""

	CI = pd.read_json(join(INPUTS_FOLDER, "CI.json"))
	TA_Districts = pd.read_json(join(INPUTS_FOLDER, "TA_Districts.json"))

	with open(join(INPUTS_FOLDER, "TA_adj_Dist.json")) as json_file: 
		TA_adj_Dist = json.load(json_file)

	with open(join(INPUTS_FOLDER, "TA_adj_TA.json")) as json_file: 
		TA_adj_TA = json.load(json_file)

	return CI, TA_Districts, TA_adj_Dist, TA_adj_TA



def populate_inputs_folders():
	"""

	"""

	print("preparing data...")
	CI, adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict = go()

	print("outputting files...")
	CI.to_json(join(INPUTS_FOLDER, "CI.json"))
	adm3_homes.to_json(join(INPUTS_FOLDER, "TA_Districts.json"))

	with open(join(INPUTS_FOLDER, "TA_adj_Dist.json"), 'w') as json_file:
		json.dump(adm3_to_adm2_dict, json_file)

	with open(join(INPUTS_FOLDER, "TA_adj_TA.json"), 'w') as json_file:
		json.dump(adm3_to_adm3_dict, json_file)


def create_relations():
	"""
	"""

	### adm2 file
	adm2 = gpd.read_file(join(DATA_FOLDER, SHAPE_FILES[0]))  ## load file
	adm2 = adm2[["ADM2_PCODE", "geometry"]].rename({"ADM2_PCODE": "ADM2"}, axis=1) ## subset and rename cols
	adm2["ADM2"] = adm2["ADM2"].str.strip()

	### adm3 file
	adm3 = gpd.read_file(join(DATA_FOLDER, SHAPE_FILES[1]))
	adm3 = adm3[["ADM3_PCODE", "ADM2_PCODE", "ADM2_EN", "ADM3_EN", "geometry"]].rename({"ADM3_PCODE": "ADM3", \
		"ADM2_PCODE": "ADM2"}, axis=1)
	adm3["ADM2"] = adm3["ADM2"].str.strip()
	adm3["ADM3"] = adm3["ADM3"].str.strip()

	adm3_homes = adm3[["ADM2", "ADM3", "ADM2_EN", "ADM3_EN"]]

	### connect adm3s to 2s
	tmp = gpd.sjoin(adm3, adm2, how='left', op='intersects') ## spacial join
	adm3_to_adm2 = tmp[tmp["ADM2_left"] != tmp["ADM2_right"]] ## filter out matches with home adm

	adm3_to_adm2_dict = df_to_dict(adm3_to_adm2[["ADM3", "ADM2_right"]])

	### connect adjacent adm3s
	tmp = gpd.sjoin(adm3, adm3, how="left", op='intersects')
	adm3_to_adm3 = tmp.loc[tmp["ADM3_left"] != tmp["ADM3_right"], ["ADM3_left", "ADM3_right"]]
	adm3_to_adm3_dict = df_to_dict(adm3_to_adm3[["ADM3_left", "ADM3_right"]])

	return adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict


def import_current_infections():

	tmp = pd.read_csv(join(DATA_FOLDER, CURRENT_INFECTIONS))
	loc_var = "ADM3_PCODE" if "ADM3_PCODE" in tmp.columns else "ADM2_PCODE" 
	CI = tmp[[loc_var, "Current Infections"]]
	CI[loc_var] = CI[loc_var].str.strip() 	

	return CI.set_index(loc_var)


def df_to_dict(df):
	'''
	Creates a dictionary from a df with 2 columns
	First column becomes key, second becomes value
	'''

	d = {}

	for k, v in df.itertuples(index=False, name=None):
		d[k] = d.get(k, []) + [v]

	return d


def get_params():

	while True:
		val = input("How many adjacent TAs away should be searched: ")

		try:
			degree = int(val)
			if degree < 1:  
				print("Sorry, input must be 1 or greater, try again")
				continue
			break
		except ValueError:
			print("That is not a positive integer")

	i = 1
	multiplier_dict = {}

	while i < degree + 1:
		val = input("Enter the weight of adjacent TA {}: ".format(i))

		try:
			weight = float(val)
			if weight > 1 or weight < 0:
				print("Sorry, between 1 and 0, try again")
				continue
			multiplier_dict[i] = weight	
			i += 1
			
		except ValueError:
			print("That is not between 0 and 1")
	print()

	print("Executing analysis for {} degrees".format(degree))
	print("Here are the weights:")
	for k, v in multiplier_dict.items():
		print("TA {}: {}".format(k, v))
	print()
	outfile = input("Enter the name of the file you'd like to output.\n" +
		"Do not include '.csv.' or any other file extension:")

	return degree, multiplier_dict, outfile