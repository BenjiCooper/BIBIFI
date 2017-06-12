#! /usr/bin/env/python
#
#	Author: Benjamin Cooper
#   Perform statistical analyses over Build it Break it Fix it entries.

import argparse as ap
import os
import re


class Analysis(object):

	def __init__(self, dirname = '.', lang = 'all', analysis = 'all', verbose = False):
		self.dirname = dirname
		self.lang = lang
		self.analysis = analysis
		self.verbose = verbose
		self.pattern = ''

		if (self.lang == 'rust'):
			self.pattern += '.rs'
		elif (self.lang == 'ruby'):
			self.pattern += '.rb'
		elif (self.lang == 'python'):
			self.pattern += '.py'
		elif (self.lang == 'ocaml'):
			self.pattern += '.ml'
		else:
			self.pattern += '.' + self.lang

	def pretty_print(self, s, n1 = 0):
		s = str(s)
		n = len(s)
		print('\t'*n1)
		print('\t'*n1 + '='*(n+2))
		print('\t'*n1 + ' ' + s + ' ')
		print('\t'*n1 + '='*(n+2))
		print('\t'*n1)

	def traverse(self, dirname, n = 0):
		os.chdir(dirname)
		dirlist = os.listdir(dirname)
		stack = []
		for d in dirlist:
			if(os.path.isdir('./'+d)):
				stack.append(d)
			elif(self.lang == 'all'):
				if(d.endswith('.py') or d.endswith('.rb') or d.endswith('.rs') or 
					d.endswith('.java') or d.endswith('.c') or d.endswith('.go') or d.endswith('.ml')):
					print('\t'*n + 'analyzing ' + d)
			elif(d.endswith(self.pattern)):
				print('\t'*n + 'analyzing ' + d)

		for d in stack:
			self.pretty_print(d, n+1)
			os.chdir(d)
			self.traverse('.', n+1)
			os.chdir('..')
			self.pretty_print('end '+d, n+1)

	def analyze(self):
		if(self.dirname == '.'):
			self.pretty_print('curr dir')
			self.traverse(self.dirname)
			self.pretty_print('end curr dir')
		else:
			self.pretty_print(self.dirname)
			self.traverse(self.dirname)
			self.pretty_print('end ' + self.dirname)



def main():
	parser = ap.ArgumentParser(description='Applying statistical analyses')

	parser.add_argument('-d', '--dirname', nargs=1, type = str, default = ['.'],
		help = 'The name of the directory to traverse.')

	parser.add_argument('-l', '--language', nargs=1, choices=['all','java','c','rust','go','python','ruby', 'ocaml'], default = ['all'],
		help = 'The language you would like to check. Default is all.')

	parser.add_argument('-a', '--analysis', nargs=1, choices=['all', 'linreg'], default = ['all'],
		help = 'The type of analysis you would like to apply. Default is all.')

	parser.add_argument('-v', '--verbose', action="store_true", default = False,
		help = 'Flag to to activate verbose printing.')

	args = vars(parser.parse_args())

	dirname = args['dirname'][0]
	language = args['language'][0]
	analysis = args['analysis'][0]
	verbose = args['verbose']
	a = Analysis(dirname, language, analysis, verbose)
	a.analyze()

if __name__ == '__main__':
	main()
