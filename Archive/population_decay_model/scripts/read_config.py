import pandas as pd
from os.path import join, abspath, dirname, exists

COUNTRY_INPUTS = join(dirname(dirname(abspath(__file__))), 'country_inputs')

ARGS = ['date', 'country', 'num adjacent degrees', 'current infections filename'
		'exclusions', 'bundles']

def read_config(path):
	"""
	reads config file
	inputs (str): path of config file
	returns dict of args
	"""
	with open(path, 'r') as f:
		lines = f.readlines()

	args = parse_config(lines)
	check_essential_args(args)
	check_multipliers(args)
	check_files(args)

	return args


def check_essential_args(args):
	"""
	verifies essential args are present
	"""
	if 'CI' not in args:
		raise Exception('No CI in config file')
	if 'shape' not in args:
		raise Exception('No shape in config file')
	for i in ('CI', 'CI region colname', 'country', 'shape', 'region colname'):
		if i not in args:
			raise Exception('Mandatory arg not found in config file.'
							'Please ensure the following args are populated:'
							' CI, CI region colname, country, shape, region colname')

def check_files(args):
	"""
	verifies files exist
	raises error if verification fails
	"""

	for k, v in args.items():
		if k in ('CI', 'exclusions', 'bundles', 'shape'):
			if not exists(v):
				raise Exception("{} not found".format(v))


def check_multipliers(args):
	"""
	verifies correct num adjacent degrees and multipliers
	raises error if verification fails
	"""
	n = args.get('num adjacent degrees')
	if not n:
		raise Exception('Must have num adjacent degrees in config file')

	for i in range(n):
		if ' '.join(['multiplier', str(i+1)]) not in args.keys():
			raise Exception("Number of multiplier arguments must be match num adjacent degrees in config file")


def parse_config(lines):
	"""
	builds dictionary from config file arguments
	inputs:
		lines: list of lines read from config file
	returns dict of args
	"""

	args = {}
	for line in lines:

		l = line.split(':')
		k = l[0]
		v = l[1].strip()

		if k in ('date', 'country', 'region colname', 'CI region colname'):
			args[k] = v
		if k == 'outfile name':
			if v[-4:] == '.csv':
				args[k] = v
			else:
				args[k] = v + '.csv'
		if k == 'num adjacent degrees':
			args[k] = int(v)
		if k[:10] == 'multiplier':
			args[k] = float(v)
		if k in ('CI', 'exclusions', 'bundles', 'shape'):
			country = args.get('country')
			if not country:
				raise Exception('country must be before CI, exclusions, and bundle in config file')
			v = join(COUNTRY_INPUTS, country, k, v)
			args[k] = v

	return args



			