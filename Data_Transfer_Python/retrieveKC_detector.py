import urllib
import numpy as np
import shutil

detectormapping=open('C:\\DTAX\\python\\detector_mapping.txt','r')
detectorid=[]
lines=detectormapping.readlines()
for i in range(len(lines)):
    detectorid.append(lines[i][0:4])
detectormapping.close()
count=0
detectordata=np.zeros((3,len(detectorid)))
detector = urllib.urlopen("https://imrcp.data-env.com/detector.dat")
detectorout=open('C:\\DTAX\\run\\stcc\\detector.dat','w')  # one minute interval
lines=detector.readlines()
for i in range(1,len(lines)):
    linearray=lines[i].split()
    if linearray[0] in detectorid:
        linkid=detectorid.index(linearray[0])
        detectordata[0][linkid]=float(linearray[2])  #flow
        detectordata[1][linkid]=float(linearray[3])   #speed
        detectordata[2][linkid]=float(linearray[4])   #Occ
for i in range(len(detectorid)):
    detectorout.write(str(detectordata[0][i])+'\t'+str(detectordata[1][i])+'\t'+str(detectordata[2][i]) +'\n')
detectorout.close()

detectorout1=open('C:\\DTAX\\run\\ltcc\\detector.dat','w')  #five minute average
for i in range(1,len(lines)):
    linearray=lines[i].split()
    if linearray[0] in detectorid:
        count=count+1
        linkid=detectorid.index(linearray[0])
        detectordata[0][linkid]+=float(linearray[2])*5   #flow,transfer to veh for 5 minute. consistent with freeway detector 5 min volume
        detectordata[1][linkid]+=float(linearray[3])   #speed
        detectordata[2][linkid]+=float(linearray[4])   #Occ
for i in range(len(detectorid)):
    #print detectordata[i],i
    timecount=count*1.0/len(detectorid) #take average
    detectorout1.write(str(detectordata[0][i]/timecount)+'\t'+str(detectordata[1][i]/timecount)+'\t'+str(detectordata[2][i]/timecount) +'\n')
detectorout1.close()
 

filename3='C:\DTAX\\run\ltcc\detector.dat'
filename4='C:\DTAX\\run\odestimate\detector.dat'
shutil.copy2(filename3,filename4)
print ('detector data loaded')    
            
