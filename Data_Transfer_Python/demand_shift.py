import random
from datetime import datetime
import numpy
import shutil

timenow=datetime.now()
targettime=str(timenow.hour*60+numpy.ceil(timenow.minute/5.0)*5)
with open('timeZero.dat', 'w') as wfile:
    wfile.write(str(timenow))
wfile.close()

filename='demand_origin.dat'

filename2='C:\DTAX\\run\estimate\demand.dat'
filename3='C:\DTAX\\run\predict\demand.dat'
filename4='C:\DTAX\\run\odestimate\demand_hist.dat'
filename5='C:\DTAX\\run\odpredict\demand_hist.dat'



if __name__ == "__main__":
    
    ttfile=open(filename,'r')
    ttfile2=open(filename2,'w')
    lines=ttfile.readlines()
    ttfile2.write(lines[0])
    ttfile2.write(lines[1])
    i=2
    count=0
    while (i<len(lines)):
        #print len(lines[i])
        if targettime in lines[i]:
            flag=i
            for m in range (flag, len(lines)):
                ttfile2.write(lines[m])
            for m in range (2, flag):
                ttfile2.write(lines[m])
            break
        else:
            i=i+1
    ttfile.close()
    ttfile2.close()
shutil.copy2(filename2,filename3)
shutil.copy2(filename2,filename4)
shutil.copy2(filename2,filename5)





