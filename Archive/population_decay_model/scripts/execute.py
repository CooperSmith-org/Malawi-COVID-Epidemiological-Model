import pandas as pd
import sys
import datetime
from os.path import join, abspath, dirname, exists
import read_config
import read_files
import prepare_raw_files
import search_regions

CONFIG_FOLDER = join(dirname(dirname(abspath(__file__))), 'config')
OUTPUT_FOLDER = join(dirname(dirname(abspath(__file__))), 'output')
CONSOLIDATE_INPUTS = join(dirname(dirname(dirname(abspath(__file__)))), 'consolidation_files', 'files')


def main():

	for c in sys.argv[1:]:

		args = read_config.read_config(join(CONFIG_FOLDER, c))
		r = args['region colname']

		file_args = {k: v for k, v in args.items() if k in ('CI', 'exclusions', 'bundles', 'shape')}
		file_dict = read_files.upload_files(file_args)
		prepare_raw_files.create_intermediates(args, file_dict)

		CI, reg_map, adj_dict = read_files.read_json(args['country'])

		total_connections = search_regions.get_connections(reg_map[r],
			adj_dict, CI, args['num adjacent degrees'], r)

		multiplier_dict = read_files.get_multiplier_dict(args)
		scores = search_regions.calc_scores(total_connections, CI, multiplier_dict, r)

		outfile = args.get('outfile name', prepare_raw_files.form_outfile_name(args['country']))
		scores.to_csv(join(OUTPUT_FOLDER, outfile), index=False)


if __name__ == "__main__":

	main()
