""" This module handles all server requests. Uses web.py."""

import web
import json
from csvFun import write_to_file

web.config.debug = True
        
urls = (
	'/', 'Index',
	'/ans', 'Save_to_csv'
)

class Index:
	"""A class that handles default GET requests."""
	def GET(self):
		return 'HELLO WORLD!'

class Save_to_csv:
	"""A class that handles /ans requests to save things to CSV. For user study purposes."""
	def GET(self):
		web.header('Content-Type', 'application/json')
		web.header('Access-Control-Allow-Origin', '*')
		data = web.input()
		answers = data.answers
		write_to_file('/Users/oomeir/Documents/544-userstudy/data/user-study-data.csv', answers)
		return answers

if __name__ == "__main__": 
    app = web.application(urls, globals())
    app.run()
