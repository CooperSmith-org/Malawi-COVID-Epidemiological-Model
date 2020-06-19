from os.path import join, abspath, dirname
import pandas as pd
import geopandas as gpd
import json

COUNTRY_INPUTS = join(dirname(dirname(abspath(__file__))), 'country_inputs')


def upload_files(args):

	arg_files = {}
	for k, path in args.items():
		if k in ("bundles", "exclusions"):
			v = read_contraint(path)
		elif k == "CI":
			v = pd.read_csv(path)
		elif k == "shape":
			v = gpd.read_file(path)
		arg_files[k] = v

	return arg_files


def get_multiplier_dict(args):

	multiplier_dict = {}
	for k, v in args.items():
		if 'multiplier' in k:
			multiplier_dict[int(k[-1])] = v
	return multiplier_dict


def read_contraint(path):
	"""
	reads file and returns list
	"""
	
	with open(path, 'r') as f:
		lines = f.readlines()

	return [l.strip() for l in lines]
	

def read_json(country):
	"""
	reads files from json
	"""

	intermediates_folder = join(COUNTRY_INPUTS, country, 'intermediate_data')

	CI = pd.read_json(join(intermediates_folder, "CI.json"))
	reg_map = pd.read_json(join(intermediates_folder, "reg_map.json"))

	with open(join(intermediates_folder, "adj_dict.json")) as json_file: 
		adj_dict = json.load(json_file)

	return CI, reg_map, adj_dict


# if __name__ == '__main__':
# 	bundles = join(country_inputs, country, 'bundles')
# 	exclusions = join(country_inputs, country, 'exclusions')

# 	read_bundles(join(bundles, bundle))