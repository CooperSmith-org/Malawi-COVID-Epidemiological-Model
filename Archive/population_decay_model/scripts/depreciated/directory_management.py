from os.path import join, dirname, abspath
from os import listdir




def check_for_shp(country):

	folder = join(dirname(abspath(dirname(__file__))), "country_inputs", country, "geography")
	return ".shp" in [f[-4:] for f in listdir(folder)]

def check_int_files(country, fn):

	folder = join(dirname(abspath(dirname(__file__))), "country_inputs", country, "intermediate_data")
	return bool(listdir(folder))
