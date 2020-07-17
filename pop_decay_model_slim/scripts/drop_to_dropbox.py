import dropbox
import json
from datetime import datetime
from os.path import join

def get_access_info():
	"""
	loads authentication info
	"""
	
	with open('nothing_here.json', 'r') as f:
		access_info = json.loads(f.read())

	return access_info


def access_dbx():
	"""
	returns dropbox object
	"""

	access_info	= get_access_info()
	dbx = dropbox.Dropbox(access_info['ACCESS_TOKEN'])
	return dbx


def upload_file(df):
	"""
	uploads file to dropbox
	"""

	### changes df filetype
	df_string = df.to_csv()
	df_byte = bytes(df_string, 'utf8')

	### loads parameters for uploading file
	folder = "/Current Infections/Pop Decay Model Output"
	date = datetime.now().strftime("%Y-%M-%d_%H%M")
	path = join(folder, "decay_model_output_{}.csv".format(date))
	dbx = access_dbx()

	### uploads file
	dbx.files_upload(f=df_byte, path=path,
		mode=dropbox.files.WriteMode.overwrite) 
