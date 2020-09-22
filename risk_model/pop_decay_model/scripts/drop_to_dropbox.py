import dropbox
import json
from datetime import datetime
from os.path import join

def get_access_info(root):
	"""
	loads authentication info
	"""
	
	with open(join(root, 'secrets/drop_token.txt'), 'r') as f:
		token = f.read()

	return token


def access_dbx(root):
	"""
	returns dropbox object
	"""

	token	= get_access_info(root)
	dbx = dropbox.Dropbox(token)
	return dbx


def upload_pop_decay(df, root):
	"""
	uploads file to dropbox
	"""

	### changes df filetype
	df_string = df.to_csv()
	df_byte = bytes(df_string, 'utf8')

	### loads parameters for uploading file
	folder = "/Current Infections/Pop Decay Model Output"
	date = datetime.now().strftime("%Y-%m-%d_%H%M")
	path = join(folder, "decay_model_output_{}.csv".format(date))
	dbx = access_dbx(root)

	### uploads file
	print(path)
	dbx.files_upload(f=df_byte, path=path,
		mode=dropbox.files.WriteMode.overwrite)


def upload_risk_model(df, root):
	"""
	uploads file to dropbox
	"""

	### changes df filetype
	df_string = df.to_csv()
	df_byte = bytes(df_string, 'utf8')

	### loads parameters for uploading file
	folder = "/Risk Model Output"
	date = datetime.now().strftime("%Y-%m-%d_%H%M")
	path = join(folder, "Risk_Model_{}.csv".format(date))
	dbx = access_dbx(root)

	### uploads file
	print(path)
	dbx.files_upload(f=df_byte, path=path,
		mode=dropbox.files.WriteMode.overwrite) 
