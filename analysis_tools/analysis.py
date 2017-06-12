#! /usr/bin/env/python
#
#	Author: Benjamin Cooper
#   Perform statistical analyses over Build it Break it Fix it entries.

from scipy import stats
import argparse as ap
import random as rnd
import os
import re


class Analysis(object):

	def __init__(self, dirname = '.', lang = 'all', analysis = 'all', verbose = False):
		self.dirname = dirname
		self.lang = lang
		self.analysis = analysis
		self.verbose = verbose
		self.pattern = ''
		self.results = []

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

	def cyclomatic(self, filename):
		# TODO: have this apply OCaml code and return cyclomatic complexity
		return rnd.randint(0,100)

	def security(self, filename):
		# TODO: have this look up the security score for the given file
		return rnd.randint(0,100)

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
					cyclo = self.cyclomatic(d)
					secu = self.security(d)
					self.results.append((cyclo,secu))
					if(self.verbose):
						print('\t'*n + 'analyzing ' + d + '...' + str((cyclo,secu)))
			elif(d.endswith(self.pattern)):
				cyclo = self.cyclomatic(d)
				secu = self.security(d)
				self.results.append((cyclo,secu))
				if(self.verbose):
					print('\t'*n + 'analyzing ' + d + str((cyclo,secu)))


		for d in stack:
			if(self.verbose):
				self.pretty_print(d, n+1)
			os.chdir(d)
			self.traverse('.', n+1)
			os.chdir('..')


	def analyze(self):
		if(self.verbose):
			if(self.dirname == '.'):
				self.pretty_print('curr dir')
				self.traverse(self.dirname)
			else:
				self.pretty_print(self.dirname)
				self.traverse(self.dirname)
			slope, intercept, r_value, p_value, std_err = stats.linregress(self.results)
			print(self.results)
			self.pretty_print('linear regression')
			print('slope: ' + str(slope))
			print('intercept: ' + str(intercept))
			print('r_value: ' + str(r_value))
			print('p_value: ' + str(p_value))
			print('std_err: ' + str(std_err))
		else:
			self.traverse(self.dirname)
			slope, intercept, r_value, p_value, std_err = stats.linregress(self.results)
			self.pretty_print('linear regression')
			print('slope: ' + str(slope))
			print('intercept: ' + str(intercept))
			print('r_value: ' + str(r_value))
			print('p_value: ' + str(p_value))
			print('std_err: ' + str(std_err))
			

def main():
	parser = ap.ArgumentParser(description='Applying statistical analyses to BIBIFI submissions')

	parser.add_argument('-d', '--dirname', nargs=1, type = str, default = ['.'],
		help = 'The name of the directory to traverse.')

	parser.add_argument('-l', '--language', nargs=1, choices=['all','java','c','rust','go','python','ruby', 'ocaml'], default = ['all'],
		help = 'The language you would like to check. Default is all.')

	parser.add_argument('-a', '--analysis', nargs=1, choices=['all', 'linreg'], default = ['all'],
		help = 'The type of analysis you would like to apply. Default is all. (note: as of now it will only do linreg)')

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
