""" In this module funtions that write and read from csv and other format files """
import csv

def write_to_file(f, dat):
	writer = open(f, 'a')
	writer.write(dat + '\n')
	writer.close()