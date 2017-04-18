import os

targetlinkid1=[3177,3359,3218,2900,3175,2752,2900,2177,2865,1774,2174,1647,1926,1249,1771,820,1644,803,1246]
targetlinkid2=[3201,3215,2903,3214,2906,2912,2412,2904,2404,2418,1835,2170,1749,1867,1649,1653,1225,1277,822,837]
path='C:\Users\Zihan\Desktop\AMS\Simulation\OC1' 
path=os.getcwd()

targetlinkid=targetlinkid2[20:22]

#targetlinkid=[2900,3175]
def linkvol():
    speedfile=open(path+'\\'+'OutLinkSpeedAll.dat','r')
    #speedout=open('out'+'linkspeed.csv','w')
    volfile=open(path+'\\'+'LinkVolume.dat','r')
    #volout=open('outlinkvol.csv','w')
    lines=speedfile.readlines()
    lines2=volfile.readlines()
    L=[]
    S=[]
    i=7
    count=0
    while (count<1440):
        speed = [map(float,line.split( )) for line in lines[i:i+480]]
        vol = [map(float,line.split( )) for line in lines[i:i+480]]
        for lid in targetlinkid:
            m=lid/10
            n=lid-lid/10-1
            L.append(vol[m][n])
            S.append(speed[m][n])
        i=i+484
    volfile.close
    speedfile.close
    print sum(L)
    print sum(S)
    print ('over')
    return

#linkvol()


speedfile=open(path+'\\'+'OutLinkSpeedAll.dat','r')
#speedout=open('out'+'linkspeed.csv','w')
volfile=open(path+'\\'+'LinkVolume.dat','r')
#volout=open('outlinkvol.csv','w')
lines=speedfile.readlines()
lines2=volfile.readlines()
L=[]
S=[]
i=7
count=0
while (count<300):
    speed = [map(float,line.split( )) for line in lines[i:i+480]]
    vol = [map(float,line.split( )) for line in lines2[i:i+480]]
    for lid in targetlinkid:
        m=lid/10
        n=lid-lid/10*10-1
        L.append(vol[m][n])
        S.append(speed[m][n])
    i=i+484
    count=count+1
print sum(L)
# print sum(S)/100

L=[]
while (count<540):
    speed = [map(float,line.split( )) for line in lines[i:i+480]]
    vol = [map(float,line.split( )) for line in lines2[i:i+480]]
    for lid in targetlinkid:
        m=lid/10
        n=lid-lid/10*10-1
        L.append(vol[m][n])
        S.append(speed[m][n])
    i=i+484
    count=count+1
print sum(L)

L=[]
while (count<15*60):
    speed = [map(float,line.split( )) for line in lines[i:i+480]]
    vol = [map(float,line.split( )) for line in lines2[i:i+480]]
    for lid in targetlinkid:
        m=lid/10
        n=lid-lid/10*10-1
        L.append(vol[m][n])
        S.append(speed[m][n])
    i=i+484
    count=count+1
print sum(L)

L=[]
while (count<19*60):
    speed = [map(float,line.split( )) for line in lines[i:i+480]]
    vol = [map(float,line.split( )) for line in lines2[i:i+480]]
    for lid in targetlinkid:
        m=lid/10
        n=lid-lid/10*10-1
        L.append(vol[m][n])
        S.append(speed[m][n])
    i=i+484
    count=count+1
print sum(L)

L=[]
while (count<1440):
    speed = [map(float,line.split( )) for line in lines[i:i+480]]
    vol = [map(float,line.split( )) for line in lines2[i:i+480]]
    for lid in targetlinkid:
        m=lid/10
        n=lid-lid/10*10-1
        L.append(vol[m][n])
        S.append(speed[m][n])
    i=i+484
    count=count+1
print sum(L)
volfile.close
speedfile.close
print ('over')
