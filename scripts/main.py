import data_loading
import classes
 
def main():

	df, adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict = data_loading.go()
	print(df)
	TA_dict = classes.build_TAs(df, adm3_to_adm2_dict, adm3_to_adm3_dict)

	return TA_dict