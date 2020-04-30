from os.path import join, dirname, abspath
import data_loading
import classes
import pandas as pd
 
OUTPUT_FOLDER = join(dirname(abspath(dirname(__file__))), "output")


def main(degree, first_multiplier, second_multiplier, conn_fn=None, score_fn=None):

	if not(conn_fn and score_fn):
		print("No output filenames provided.  Please use the conn_fn or score_fn arguments")

	df, adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict = data_loading.go()

	connectiosn = find_connections
	
	d, t = classes.build_objects(df, adm3_to_adm2_dict, adm3_to_adm3_dict)
	classes.build_adjacencies(d, t, adm3_to_adm2_dict, adm3_to_adm3_dict)
	conns = classes.get_connections(d, t)
	conns_df = classes.convert_connections_to_df(conns, first_multiplier, second_multiplier)
	scores = classes.calc_total_scores(t, first_multiplier, second_multiplier)
	scores_df = pd.DataFrame(scores, columns=['TA', 'Score', 'From CI'])

	if conn_fn:
		conns_df.to_csv(join(OUTPUT_FOLDER, conn_fn), index=False)
	if score_fn:
		scores_df.to_csv(join(OUTPUT_FOLDER, score_fn), index=False)
