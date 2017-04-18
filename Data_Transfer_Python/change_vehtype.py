import random
random.seed(0)

path='C:\Users\Zihan\Desktop\AMS\Simulation\Simulations_May24\OC1-TEST\OC1-ADM-5-15-0-0.5'
market=0.5


filename='vehicle'
def pene():
#if __name__ == "__main__":
    ttfile=open(path+'\\'+'vehicle.dat','r')
    ttfile2=open(path+'\\'+'newvehicle.dat','w')
    lines=ttfile.readlines()
    ttfile2.write(lines[0])
    vehnum=int(lines[0].split()[0])
    vehids=random.sample(range(1,vehnum+1),int(0.5*vehnum))
    ttfile2.write(lines[1])
    i=2
    while (i<len(lines)-2):
        if i/2 not in vehids:
            ttfile2.write(lines[i][0:64]+'0'+lines[i][65:-1]+'\n')
            ttfile2.write(lines[i+1])
        else:
            ttfile2.write(lines[i][0:64]+'1'+lines[i][65:-1]+'\n')
            ttfile2.write(lines[i+1])
        i=i+2
    ttfile2.write(lines[-1])
    ttfile.close
    ttfile2.close


path='C:\Users\Zihan\Desktop\AMS\Simulation\Simulations_May24\OC3-TEST\OC3-donothing-veh'


filename='vehicle'
if __name__ == "__main__":
    ttfile=open(path+'\\'+'vehicle.dat','r')
    ttfile2=open(path+'\\'+'newvehicle.dat','w')
    lines=ttfile.readlines()
    ttfile2.write(lines[0])
    ttfile2.write(lines[1])
    i=2
    while (i<len(lines)-2):
        ttfile2.write(lines[i][0:64]+'0'+lines[i][65:-1]+'\n')
        ttfile2.write(lines[i+1])
        i=i+2
    ttfile2.write(lines[-1])
    ttfile.close
    ttfile2.close
 
 





