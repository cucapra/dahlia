#!/usr/bin/python

#import subprocess
import re
import os
import sys
import csv


def synth_data(task):
    ################ Open CSV files to write results to ##########################
    datacsvpath = os.path.join(task.code_dir, 'run_data.csv')
    datacsv = open(datacsvpath,'w') #overwrite existing
    datawriter = csv.writer(datacsv)
    task.log(" ")
    task.log("----------------------------------------------------------------")
    task.log("--    Data extraction stage    --")
    task.log("----------------------------------------------------------------")
    
    hw_rpt(datawriter, task)
    if task['config'].get('estimate'):
        pf_rpt(datawriter, task)
    #else:
        #hw_rpt(datawriter, task)
        #md_rpt(datawriter, task)
    
    dm_rpt(datawriter, task)

    # Close file
    datacsv.close()
    

def test_extract(task):
    task.log('Testing extract script worker access')


def hw_rpt(datawriter, task):
    ################ Open hardware module file ###################################
    task.log(" ")
    task.log("--     hardware preformance data    --")
    hw_basename = task['config']['hwname'] # if task['config']['make'] # else task['hw_basename']
    report = os.path.join(task.code_dir, '_sds/reports', 'sds_' + hw_basename + '.rpt')
    if os.path.isfile(report): # Use with for file open
        data = open(report, "r+")
    
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
        
        task.log ('Target Clock, Estimated Clock, Min Latency, Max Latency, Pipelining')
        task.log (timing[2] + ', ' + timing[3] + ', ' + latency[1] + ', ' + latency[2] + ', ' + latency[5])
        task.log(" ")
        
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
            task.log (names[num] + '- ' + total[num] + ' of ' + avail[num] + ' -> ' + util[num] + ' %')
            datawriter.writerow([names[num],total[num],avail[num],util[num]])
        datawriter.writerow([ ])
        
        # Close file
        data.close()
    
    else:
        task.log("sds_" + hw_basename + " does not exit!! @ " + report)
        #sys.exit()
    

def pf_rpt(datawriter, task):
    ################ Open hardware estimate file ###################################
    task.log(" ")
    task.log("--    hardware preformance estimates    --")
    report = os.path.join(task.code_dir, '_sds/est', 'perf.est')
    if os.path.isfile(report): # Use with for file open
        data = open(report, "r+")
    
        lines = []
        for line in data:
            lines.append(line)
        
        # extract relevant data
        if lines[2].find("hwLatency")>0:
            latency = re.findall('(?<=")[^"]*(?=")',lines[2])
            task.log('hw cycles' + " " + latency[0])
        
        if lines[6].find("dsp")>0:
            dsp     = re.findall('(?<=")[^"]*(?=")',lines[6])
            task.log(dsp[2] + " / " + dsp[4])
        
        if lines[7].find("bram")>0:
            bram    = re.findall('(?<=")[^"]*(?=")',lines[7])
            task.log(bram[2] + " / " + bram[4])
        
        if lines[8].find("lut")>0:
            lut     = re.findall('(?<=")[^"]*(?=")',lines[8])
            task.log(lut[2] + " / " + lut[4])
        
        if lines[9].find("ff")>0:
            ff      = re.findall('(?<=")[^"]*(?=")',lines[9])
            task.log(ff[2] + " / " + ff[4])
       
        datawriter.writerow([latency[0],dsp[2],bram[2],lut[2],ff[2]])
        
        # Close file
        data.close()
    
    else:
        task.log("perf.est does not exit!! @ " + report)
        #sys.exit()


def dm_rpt(datawriter, task):    
    ################ Open data motion network file ###############################
    task.log(" ")
    task.log("--    data motion network    --")
    report = os.path.join(task.code_dir, '_sds/reports', 'data_motion.html')
    if os.path.isfile(report):  # Use with for file open
        data = open(report, "r+")
    
        lines = []
        for line in data:
            if 'li' in line: # This removes additional lines added to the report for a certain pragma (update this comment)
                task.log(line)
            else:
                lines.append(line)
        
        items = 0
        for num,line in enumerate(lines,1): # section jump strategy?
            if 'Data Motion Network' in line: # Some data appear in this section
                dmn = num-1
            if 'Accelerator Callsites' in line: # Some data appear in this section
                acs = num-1
            if 'PORT' in line: # Assume every interface will have an IP Port called PORT something
                items += 1
        
        items = items//2 # IP Port appears twice in the report for the two tables
        
        task.log(" ")
        task.log ('IP port - Connection - Transfer size - Mem layout - Data mover setup time - Transfer time')
        
        datawriter.writerow(['IP port','Connection','Transfer size','Mem layout','Data mover setup time','Transfer time'])
        
        # loop over port number
        for item in range(items):
            ip   = re.findall('(?<=>).*(?=<)',lines[dmn+13+10*item])
            conn = re.findall('(?<=>).*(?=<)',lines[dmn+19+10*item])
            tsiz = re.findall('(?<=>).*(?=<)',lines[acs+14+8*item])
            meml = re.findall('(?<=>).*(?=<)',lines[acs+15+8*item])
            dmst = re.findall('(?<=>).*(?=<)',lines[acs+16+8*item])
            trft = re.findall('(?<=>).*(?=<)',lines[acs+17+8*item])
            task.log(ip[0] + ' - ' + conn[0] + ' - ' + tsiz[0] + ' - ' + meml[0] + ' - ' + dmst[0] + ' - ' + trft[0])
            datawriter.writerow([ip[0],conn[0],tsiz[0],meml[0],dmst[0],trft[0]])
        
        # Close file
        data.close()
        
    else:
        task.log("data_motion does not exit!! @ " + report)
        #sys.exit()


def runtime_data(datawriter, task):
    
    ######### Open json ##########
    task.log(" ")
    task.log("--    run data extraction not yet implemented    --")
