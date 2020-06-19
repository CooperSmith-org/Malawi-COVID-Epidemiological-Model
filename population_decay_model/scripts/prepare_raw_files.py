from os.path import join, abspath, dirname
from datetime import date
import pandas as pd
import geopandas as gpd
import json
import warnings

COUNTRY_INPUTS = join(dirname(dirname(abspath(__file__))), 'country_inputs')

warnings.filterwarnings("ignore")

class Constraint(object):

	def __init__(self, kind, l, r):
		"""
		r: region of interest for analysis
		kind: bund or excl
		r_constraint: region of contraint
		l: list built from constraint file
		"""
		self.kind = kind
		self.r = r
		self.r_constraint, self.l = self.parse_list(l)

	def parse_list(self, l):

		r_constraint = l.pop(0)
		return r_constraint, l

	def apply_constraint(self, df):

		if self.kind == 'bundles':
			df.loc[df[self.r_constraint].isin(self.l), self.r] = \
			df.loc[df[self.r_constraint].isin(self.l), self.r_constraint]

		elif self.kind == 'exclusions':
			df = df.loc[~df[self.r_constraint].isin(self.l), :]

		return df


def create_intermediates(args, file_dict):
	"""
	"""
	
	intermediates_dict = {}

	constraints = []
	for k, v in file_dict.items():
		if k in ('bundles', 'exclusions'):
			c = Constraint(k, v, args['region colname'])
			constraints.append(c)

	adj_dict, reg_map, r_list = create_adj_dict(args, file_dict, constraints)

	merge = args['CI region colname'] != args['region colname']
	merge_df = reg_map if merge else None

	CI = clean_CI(file_dict['CI'], args['CI region colname'], constraints, merge, merge_df)

	write(args['country'], CI, reg_map, adj_dict)


def extract_constrain_info(l):
	"""
	inputs:
		list
	returns:
	colname, list
	"""

	r_bund = bund_list.pop(0)
	regions.add(r_bund)
	bund = True


def clean_CI(CI, colname, constraints, merge=False, merge_df=None):
	"""
	CI: df of CIs
	r_list: list of relevant regions
	constraints:
	"""

	CI = CI[[colname, 'Current Infections']]
	if merge:
		CI = CI.merge(merge_df, how='inner', left_on=colname, right_on=colname)

	for c in constraints:
		CI = c.apply_constraint(CI)

	return CI


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


def create_adj_dict(args, file_dict, constraints):
	"""
	builds adj dictionary from file_dict and arguments
	needs to be broken into functions
	"""

	shape = file_dict['shape']
	r = args['region colname']
	r_CI = args['CI region colname']

	regions = list(set([r, r_CI] + [c.r_constraint for c in constraints]))
	reg_map = shape[regions + ['geometry']]

	for c in constraints:
		reg_map = c.apply_constraint(reg_map)

	### connect adjacent adm3s
	adj = spatial_selfjoin(reg_map, r)
	adj_dict = df_to_dict(adj)

	reg_map = reg_map.drop('geometry', axis=1).drop_duplicates(ignore_index=True)

	return adj_dict, reg_map, regions


def spatial_selfjoin(df, col):

	tmp = gpd.sjoin(df[[col, 'geometry']], df[[col, 'geometry']], how="left", 
		op='intersects', lsuffix='orig', rsuffix='adj')
	adj = tmp.loc[tmp[col + '_orig'] != tmp[col + '_adj'], [col + '_orig', col + '_adj']]
	adj = adj[[col + '_orig', col + '_adj']].drop_duplicates(ignore_index=True)
	return adj


def write(country, CI, reg_map, adj_dict):

	intermediates_folder = join(COUNTRY_INPUTS, country, 'intermediate_data')

	CI.to_json(join(intermediates_folder, 'CI.json'))
	reg_map.to_json(join(intermediates_folder, 'reg_map.json'))

	with open(join(intermediates_folder, 'adj_dict.json'), 'w') as f:
		json.dump(adj_dict, f)


def form_outfile_name(country):
	"""
	creates name of outfile
	"""

	outfile = '_'.join(['decay_model', country, str(date.today()), '.csv'])

	return outfile


# def grab_reg_names(file_dict
# 	r_names = {}

# 	r_names['r'] = file_dict['region colname']
# 	r_names['r_CI'] = file_dict['CI']

# 	bund_list = file_dict.get('bundles')
# 	if bund_list:
# 		r_names['r_bund'] = bund_list.pop(0)

# 	excl_list = file_dict.get('exclusions')
# 	if excl_list:
# 		r_names['r_excl'] = excl_list.pop(0)

# 	return r_names