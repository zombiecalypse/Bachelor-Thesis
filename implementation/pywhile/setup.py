from setuptools import setup
from pywhile import version

setup(
	name = 'PyWhile',
	version = version,
	description = 'Implementation of the WHILE programming language for '+
    'computabiliy introductions.',
	author = "Aaron Karper",
	author_email = "maergil@gmail.com",
	packages = ['pywhile'],
    scripts = ['bin/while.py'],
    license = 'LICENSE.txt',
    long_description = open('README.txt').read(),
	install_requires = [
        'pyparsing >= 1.5.6'
        ]
    )
