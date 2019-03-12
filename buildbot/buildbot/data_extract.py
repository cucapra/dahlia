#!/usr/bin/python

#import subprocess
import re
import os
import sys
import csv

def synth_data(func_hw):
    ################ Open CSV files to write results to ##########################
    datacsv = open('run_data.csv','w') #overwrite existing
    datawriter = csv.writer(datacsv)
    
    hw_rpt(datawriter, func_hw)
    dm_rpt(datawriter)
    md_rpt(datawriter)

    # Close file
    data.close()
    

def hw_rpt(datawriter, func_hw):
    ################ Open hardware module file ###################################
    print("--hardware preformance data--")
    report = os.path.join(os.getcwd(), '_sds/reports', 'sds_' + func_hw + '.rpt')
    if os.path.isfile(report): # Use with for file open
        data = open(report, "r+")
    else:
        print("sds_" + func_hw + " does not exit!! @ " + report)
        sys.exit()
    
    lines = []
    for line in data:
        lines.append(line)
    
    # extract relevant data # section jump strategy?
    for num, line in enumerate(lines,1):
        if '== Performance Estimates' in line:
            lat = num-1
        if '== Utilization Estimates' in line:
            utl = num-1
    
    # Read timing details
    timing  = re.split("\|",''.join(lines[lat+7].split()))
    latency = re.split("\|",''.join(lines[lat+16].split()))
    
    print ('Target Clock, Estimated Clock, Min Latency, Max Latency, Pipelining')
    print (timing[2],timing[3],latency[1],latency[2],latency[5])
    
    datawriter.writerow(['Target Clock','Estimated Clock','Min latency','Max latency','Pipelining'])
    datawriter.writerow([timing[2],timing[3],latency[1],latency[2],latency[5]])
    datawriter.writerow([ ])
    
    # Read loop details
    datawriter.writerow([ ])
    
    # Read utilization details
    for num, line in enumerate(lines[utl:utl+20],1):
        if 'Name' in line:
            names = re.split("\|+",''.join(line.split()))[1:-1] # strip away empty elements added
        if 'Total' in line:
            total = re.split("\|+",''.join(line.split()))[1:-1]
        if 'Available' in line:
            avail = re.split("\|+",''.join(line.split()))[1:-1]
        if 'Utilization' in line:
            util  = re.split("\|+",''.join(line.split()))[1:-1]
    
    for num in range(len(names)):
        print (names[num],'-',total[num],'of',avail[num],'->',util[num],'%')
        datawriter.writerow([names[num],total[num],avail[num],util[num]])
    datawriter.writerow([ ])
    
def dm_rpt(datawriter):    
    ################ Open data motion network file ###############################
    print("--data motion network--")
    report = os.path.join(os.getcwd(), '_sds/reports', 'data_motion.html')
    if os.path.isfile(report):  # Use with for file open
        data = open(report, "r+")
    else:
        print("data_motion does not exit!! @ " + report)
        sys.exit()
    
    lines = []
    for line in data:
        if 'li' in line: # This removes additional lines added to the report for a certain pragma (update this comment)
            print(line)
        else:
            lines.append(line)
    
    items = 0
    for num,line in enumerate(lines,1): # section jump strategy?
        if 'Data Motion Network' in line: # Some data appear in this section
            data = num-1
        if 'Accelerator Callsites' in line: # Some data appear in this section
            acs = num-1
        if 'PORT' in line: # Assume every interface will have an IP Port called PORT something
            items += 1
    
    items = items//2 # IP Port appears twice in the report for the two tables
    
    print ('IP port- Connection- Transfer size- Mem layout- Data mover setup time- Transfer time')
    
    datawriter.writerow(['IP port','Connection','Transfer size','Mem layout','Data mover setup time','Transfer time'])
    
    # loop over port number
    for item in range(items):
        ip   = re.findall('(?<=>).*(?=<)',lines[data+13+10*item])
        conn = re.findall('(?<=>).*(?=<)',lines[data+19+10*item])
        tsiz = re.findall('(?<=>).*(?=<)',lines[acs+14+8*item])
        meml = re.findall('(?<=>).*(?=<)',lines[acs+15+8*item])
        dmst = re.findall('(?<=>).*(?=<)',lines[acs+16+8*item])
        trft = re.findall('(?<=>).*(?=<)',lines[acs+17+8*item])
        print(ip[0],conn[0],tsiz[0],meml[0],dmst[0],trft[0])
        datawriter.writerow([ip[0],conn[0],tsiz[0],meml[0],dmst[0],trft[0]])

#def runtime_data():
    
    ######### Open json ##########
