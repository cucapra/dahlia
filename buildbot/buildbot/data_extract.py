#!/usr/bin/python

import subprocess
#from subprocess import call
import re
import os
import sys
import csv

def test_extract(task):
    task.log('Testing extract script worker access')

def synth_data(task):

	################ Open CSV files to write results to ##########################
	hwfcsv = open('hwperf.csv','w') #overwrite existing
	dmncsv = open('data_motion.csv','w')
	hwfwriter = csv.writer(hwfcsv)
	dmnwriter = csv.writer(dmncsv)
	
	# write headers
	
	################ Open hardware module file ###################################
	task.log("--hardware preformance data--")
	if os.path.isfile("_sds/reports/sds_gemm.rpt"):
		hwf = open("_sds/reports/sds_gemm.rpt", "r+")
	else:
		task.log("sds_gemm does not exit!!")
		#sys.exit()
	
	lines = []
	for line in hwf:
		lines.append(line)
	
	# extract relevant data
	for num, line in enumerate(lines,1):
		if '== Performance Estimates' in line:
			LAT = num-1
		if '== Utilization Estimates' in line:
			UTL = num-1
	
	# Read timing details
	TIMING  = re.split("\|",''.join(lines[LAT+7].split()))
	LATENCY = re.split("\|",''.join(lines[LAT+16].split()))
	
	task.log ('Target Clock, Estimated Clock, Min Latency, Max Latency, Pipelining')
	task.log (TIMING[2],TIMING[3],LATENCY[1],LATENCY[2],LATENCY[5])
	
	hwfwriter.writerow(['Target Clock','Estimated Clock','Min latency','Max latency','Pipelining'])
	hwfwriter.writerow([TIMING[2],TIMING[3],LATENCY[1],LATENCY[2],LATENCY[5]])
	hwfwriter.writerow([ ])
	
	# Read loop details
	hwfwriter.writerow([ ])
	
	# Read utilization details
	for num, line in enumerate(lines[UTL:UTL+20],1):
		if 'Name' in line:
			NAMES = re.split("\|+",''.join(line.split()))
		if 'Total' in line:
			TOTAL = re.split("\|+",''.join(line.split()))
		if 'Available' in line:
			AVAIL = re.split("\|+",''.join(line.split()))
		if 'Utilization' in line:
			UTIL  = re.split("\|+",''.join(line.split()))
	
	items = len(NAMES)
	for num in range(1,items-1):
		task.log (NAMES[num],'-',TOTAL[num],'of',AVAIL[num],'->',UTIL[num],'%')
		hwfwriter.writerow([NAMES[num],TOTAL[num],AVAIL[num],UTIL[num]])
	hwfwriter.writerow([ ])
	
	# Close file
	hwf.close()
	
	
	################ Open data motion network file ###############################
	task.log('')
	task.log("--data motion network--")
	if os.path.isfile("_sds/reports/data_motion.html"):
		dmn = open("_sds/reports/data_motion.html", "r+")
	else:
		task.log("data_motion does not exit!!")
		#sys.exit()
	
	lines = []
	for line in dmn:
		if 'li' in line:
			task.log(line)
		else:
			lines.append(line)
	
	items = 0
	for num,line in enumerate(lines,1):
		if 'Data Motion Network' in line: # Some data appear in this section
			DMN = num-1
		if 'Accelerator Callsites' in line: # Some data appear in this section
			ACS = num-1
		if 'PORT' in line: # Assume every interface will have an IP Port called PORT something
			items += 1
	
	items = items//2 # IP Port appears twice in the report for the two tables
	
	task.log ('IP port- Connection- Transfer size- Mem layout- Data mover setup time- Transfer time')
	
	dmnwriter.writerow(['IP port','Connection','Transfer size','Mem layout','Data mover setup time','Transfer time'])
	
	# loop over port number
	for item in range(items):
		IP   = re.findall('(?<=>).*(?=<)',lines[DMN+13+10*item])
		Conn = re.findall('(?<=>).*(?=<)',lines[DMN+19+10*item])
		TSiz = re.findall('(?<=>).*(?=<)',lines[ACS+14+8*item])
		MemL = re.findall('(?<=>).*(?=<)',lines[ACS+15+8*item])
		DMst = re.findall('(?<=>).*(?=<)',lines[ACS+16+8*item])
		Trft = re.findall('(?<=>).*(?=<)',lines[ACS+17+8*item])
		task.log(IP[0],Conn[0],TSiz[0],MemL[0],DMst[0],Trft[0])
		dmnwriter.writerow([IP[0],Conn[0],TSiz[0],MemL[0],DMst[0],Trft[0]])
	
	# Close file
	dmn.close()

#def runtime_data():

	######### Open json ##########
