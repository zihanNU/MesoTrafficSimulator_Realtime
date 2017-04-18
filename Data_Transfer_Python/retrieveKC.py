import urllib
from datetime import datetime
cwd='C:\\DTAX\\run'
horizon=120+15
def gettime(line):
    year=int(line[0:4])
    month=int(line[5:7])
    date=int(line[8:10])
    hh=int(line[11:13])
    mm=int(line[14:16])
    time=datetime(year,month,date,hh,mm)
    return time

def timeshift(time1,time2):
    c=time2-time1
    shift=divmod(c.days * 86400 + c.seconds, 60)[0]
    print 'timeshift,minute',shift
    return shift

timezero=open('C:\DTAX\python\\timeZero.dat','r')
lines=timezero.readlines()
initialtime=gettime(lines[0])
timezero.close()


weather = urllib.urlopen("https://imrcp.data-env.com/weather.dat")
#weatherout1=open(cwd+'\\estimate\\new_weather.dat','w')
weatherout2=open(cwd+'\\predict\\new_weather.dat','w')
currenttime=datetime.now()
lines=weather.readlines()
updatetime=gettime(lines[0])
shift1=timeshift(initialtime,updatetime)
for i in range(1,len(lines)):
    if len(lines[i])>20:
        linearray=lines[i].split()
        linearray[0]=str(max(int(linearray[0])+shift1,0))
        linearray[1]=str(int(linearray[1])+shift1)
        lines[i]='\t'+'\t'.join(linearray)+'\n'        
    weatherout2.write(lines[i])
weatherout2.close()
#weather = urllib.urlopen("https://imrcp.data-env.com/weather.dat")
#lines=weather.readlines()
#shift1=timeshift(initialtime,updatetime)
#for i in range(1,len(lines)):
 #   if len(lines[i])>20:
#        linearray=lines[i].split()
#        linearray[0]=str(max(int(linearray[0])+shift1,0))
#        linearray[1]=str(int(linearray[1])+shift1)
 #       lines[i]='\t'+'\t'.join(linearray)+'\n'
    #weatherout1.write(lines[i])
#weatherout1.close()
weather.close()


incident = urllib.urlopen("https://imrcp.data-env.com/incident.dat")
incidentout1=open(cwd+'\\estimate\\new_incident.dat','w')
incidentout2=open(cwd+'\\predict\\new_incident.dat','w')
lines=incident.readlines()
updatetime=gettime(lines[0])
shift=timeshift(initialtime,updatetime)
incidentout1.write(lines[1])
incidentout2.write(lines[1])
for i in range(2,len(lines)):
    linearray=lines[i].split()
    duration=min(float(linearray[3])-float(linearray[2]),horizon)
##    linearray[3]=str(float(linearray[2])+duration)  #avoid invalide number,for only 60minutes
##    lines[i]='\t'.join(linearray)+'\n'
##    incidentout2.write(lines[i])
    linearray[2]=str(float(linearray[2])+shift)
    linearray[3]=str(float(linearray[2])+duration)  #avoid invalide number,for only 60minutes
    lines[i]='\t'.join(linearray)+'\n'
    incidentout2.write(lines[i])
    incidentout1.write(lines[i])
incidentout1.close()
incidentout2.close()

workzone = urllib.urlopen("https://imrcp.data-env.com/workzone.dat")
workzoneout1=open(cwd+'\\estimate\\new_workzone.dat','w')
workzoneout2=open(cwd+'\predict\\new_workzone.dat','w')
lines=workzone.readlines()
updatetime=gettime(lines[0])
shift=timeshift(initialtime,updatetime)
workzoneout1.write(lines[1])
workzoneout2.write(lines[1])
for i in range(2,len(lines)):
    linearray=lines[i].split()
    duration=min(float(linearray[3])-float(linearray[2]),horizon)
##    linearray[3]=str(float(linearray[2])+duration)  #avoid invalide number,for only 60minutes
##    lines[i]='\t'+'\t'.join(linearray)+'\n'
##    workzoneout2.write(lines[i])
    linearray[2]=str(float(linearray[2])+shift)
    linearray[3]=str(float(linearray[2])+duration)  #avoid invalide number,for only 60minutes
    lines[i]='\t'+'\t'.join(linearray)+'\n'
    workzoneout2.write(lines[i])
    workzoneout1.write(lines[i])  
workzoneout1.close()
workzoneout2.close()
print ('online data loaded')
    

    
            
