import directory_management
from os.path import join, dirname, abspath
import pandas as pd
# import geopandas as gpd
import json

BUNDLED_CITIES = ["MW210", "MW315", "MW314", "MW107"]

### fancy way of setting working directory
DATA_FOLDER = join(dirname(abspath(dirname(__file__))), "data")
SHAPE_FILES = ["mwi_admbnda_adm2_nso_20181016.shp", "mwi_admbnda_adm3_nso_20181016.shp"]
CURRENT_INFECTIONS = "CurrentInfectionLocation_30April20.csv"
INT_FOLDER = join(dirname(abspath(dirname(__file__))), "country_inputs", "MW", "intermediate_data")
CI_FOLDER = join(dirname(abspath(dirname(__file__))), "country_inputs", "MW", "CI")


INPUTS_FOLDER = join(join(dirname(abspath(dirname(__file__))), "inputs"), "cleaned_data")



def load_adj(country, bundled=False, exclusions=False):
	"""
	grabs files from int folder
	"""

	int_folder = join(dirname(abspath(dirname(__file__))), "country_inputs", country, "intermediate_data")

	is_bund = "_bundled" if bundled else ""
	d_name = "excluded" if exclusions else "full"
	filename = d_name + is_bund + ".json"

	if not directory_management.check_int_files(country, filename):

		if not directory_management.check_for_shp(country):
			print("There is no shapefile for {}\nQuitting...".format(country))
			raise Exeption()
		else:
			print("Creating adjancency files...")

			#### in future have user specify file that describes bundling
			#### also have the specify file that describes exclusions
			CI_geo_col = input("Please enter the name of the geographic id in the CI file:")  ### this is needed because a df that maps regions of interest to CIs is necessary
			geo_id = input("Please enter the name of the geographic id in the shape file:")
			build_inputs.create_geo_ints(country, shape_filename, id_geo_col, CI_geo_col)

	with open(join(int_folder, filename)) as f:
		adj_dict = json.load(f)

	ids = pd.read_json(join(int_folder, "ids_" + filename))

	return adj_dict, ids


def import_CIs(CI_filename):

	CI = pd.read_csv(join(CI_FOLDER, CI_filename))

	return CI
	

def load_inputs(bundled=True):
	"""
	Loads input files from cleaned data folder
	"""

	CI = pd.read_json(join(INPUTS_FOLDER, "CI.json"))

	if bundled:

		TA_Districts = pd.read_json(join(INPUTS_FOLDER, "TA_Districts_Bundled.json"))

		with open(join(INPUTS_FOLDER, "TA_adj_Dist_Bundled.json")) as json_file: 
			TA_adj_Dist = json.load(json_file)

		with open(join(INPUTS_FOLDER, "TA_adj_TA_Bundled.json")) as json_file: 
			TA_adj_TA = json.load(json_file)

		# TA_Districts.merge(CI, how='left', right_on='ADM2', )

	else:

		TA_Districts = pd.read_json(join(INPUTS_FOLDER, "TA_Districts.json"))

		with open(join(INPUTS_FOLDER, "TA_adj_Dist.json")) as json_file: 
			TA_adj_Dist = json.load(json_file)

		with open(join(INPUTS_FOLDER, "TA_adj_TA.json")) as json_file: 
			TA_adj_TA = json.load(json_file)		

	return CI, TA_Districts, TA_adj_Dist, TA_adj_TA



def populate_inputs_folders(bundled=True):
	"""
	"""

	print("preparing data...")
	adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict = create_relations(bundled)

	is_bund = "_bundled" if bundled else ""

	CI = import_current_infections(adm3_homes)

	print("outputting files...")
	CI.to_json(join(INPUTS_FOLDER, "CI.json"))
	adm3_homes.to_json(join(INPUTS_FOLDER, "TA_Districts" + is_bund + ".json"))
	# adm3_homes_bundled.to_json(join(INPUTS_FOLDER, "TA_Districts_Bundled.json"))

	with open(join(INPUTS_FOLDER, "TA_adj_Dist" + is_bund + ".json"), 'w') as json_file:
		json.dump(adm3_to_adm2_dict, json_file)

	# with open(join(INPUTS_FOLDER, "TA_adj_Dist_Bundled.json"), 'w') as json_file:
	# 	json.dump(adm3_to_adm2_dict_bundled, json_file)

	with open(join(INPUTS_FOLDER, "TA_adj_TA" + is_bund + ".json"), 'w') as json_file:
		json.dump(adm3_to_adm3_dict, json_file)
	# with open(join(INPUTS_FOLDER, "TA_adj_TA_Bundled.json"), 'w') as json_file:
	# 	json.dump(adm3_to_adm3_dict_bundled, json_file)



def create_relations(bundled=True):
	"""
	"""

	### adm2 file
	adm2 = gpd.read_file(join(DATA_FOLDER, SHAPE_FILES[0]))  ## load file
	adm2 = adm2[["ADM2_PCODE", "geometry"]].rename({"ADM2_PCODE": "ADM2"}, axis=1) ## subset and rename cols
	adm2["ADM2"] = adm2["ADM2"].str.strip()

	### adm3 file
	adm3 = gpd.read_file(join(DATA_FOLDER, SHAPE_FILES[1]))
	adm3 = adm3[["ADM3_PCODE", "ADM2_PCODE", "ADM2_EN", "ADM3_EN", "geometry"]].\
		rename({"ADM3_PCODE": "ADM3", "ADM2_PCODE": "ADM2"}, axis=1)
	adm3["ADM2"] = adm3["ADM2"].str.strip()
	adm3["ADM3"] = adm3["ADM3"].str.strip()
	# adm3.drop_duplicates(ignore_index=True, inplace=True)


	### executes bundling
	if bundled:
		adm3.loc[adm3["ADM2"].isin(BUNDLED_CITIES), "ADM3"] = \
			adm3.loc[adm3["ADM2"].isin(BUNDLED_CITIES), "ADM2"]
		adm3.loc[adm3["ADM2"].isin(BUNDLED_CITIES), "ADM3_EN"] = \
			adm3.loc[adm3["ADM2"].isin(BUNDLED_CITIES), "ADM2_EN"]

	### builds homes
	adm3_homes = adm3[["ADM2", "ADM3", "ADM2_EN", "ADM3_EN"]]
	adm3_homes.drop_duplicates(ignore_index=True, inplace=True)

	### connect adm3s to 2s
	tmp = gpd.sjoin(adm3, adm2, how='left', op='intersects')
	adm3_to_adm2 = tmp[tmp["ADM2_left"] != tmp["ADM2_right"]] 
	adm3_to_adm2.drop_duplicates(ignore_index=True, inplace=True)

	adm3_to_adm2_dict  = df_to_dict(adm3_to_adm2[["ADM3", "ADM2_right"]])

	### connect adjacent adm3s
	tmp = gpd.sjoin(adm3, adm3, how="left", op='intersects')
	adm3_to_adm3 = tmp.loc[tmp["ADM3_left"] != tmp["ADM3_right"],
		["ADM3_left", "ADM3_right"]]
	adm3_to_adm3.drop_duplicates(ignore_index=True, inplace=False)
	adm3_to_adm3_dict = df_to_dict(adm3_to_adm3[["ADM3_left", "ADM3_right"]])

	return adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict


def import_current_infections(adm3_homes):
	"""
	imports current infections file.  can handle CIs at ADM2 or ADM3 level
	"""

	tmp = pd.read_csv(join(DATA_FOLDER, CURRENT_INFECTIONS))

	loc_var = "ADM3_PCODE" if "ADM3_PCODE" in tmp.columns else "ADM2_PCODE" 

	if loc_var == "ADM2_PCODE":
		tmp = adm3_homes.merge(tmp, how="left", 
			left_on="ADM2", 
			right_on="ADM2_PCODE")

		tmp = tmp.rename({"ADM3": "ADM3_PCODE"}, axis=1)

	tmp.loc[tmp['ADM2_PCODE'].isin(BUNDLED_CITIES), 'ADM3_PCODE'] = \
		tmp.loc[tmp['ADM2_PCODE'].isin(BUNDLED_CITIES), 'ADM2_PCODE']


	CI = tmp[["ADM3_PCODE", "Current Infections"]].drop_duplicates(ignore_index=True)

	return CI.set_index("ADM3_PCODE")


def df_to_dict(df):
	'''
	Creates a dictionary from a df with 2 columns
	First column becomes key, second becomes value
	'''

	d = {}
	# d_bundled = {}


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