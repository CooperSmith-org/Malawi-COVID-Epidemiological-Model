import data_loading
import search_TAs

DATA_FOLDER = join(dirname(abspath(dirname(__file__))), "data")


def go(outfile_name):

	degree, multiplier_dict = data_loading.get_params()

	CI, adm3_homes, adm3_to_adm2, adm3_to_adm3 = data_loading.go()

	total_cs = get_connections(adm3_list, adm3_to_adm2, adm3_to_adm3, CI, degree)

	scores = calc_scores(total_cs, CI, adm3_homes, multiplier_dict)





