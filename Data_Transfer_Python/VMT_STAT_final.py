import math
import os
I90SE=[14090,19871,14236,14296,14355,19876,14434,14556,14736,14762,14884,14945,14982]
I90NWB=[14992,14938,14837,14724,14649,19878,14464,14375,14348,14258,14173]
LakeSB=[15159,15191,15204,15216,15253,15419,15585,15776]
LakeNB=[15865,15830,15630,15485,15278,15227,15211,15198]
Pe=[13664,13915,14107,14154,14224,14343,14488,14760,14795,14921,14961,15072]

I90SEdic={'14090':0,'19871':0.4,'14236':0.77,'14296':1.1,'14355':1.43,'19876':1.73,'14434':1.93,'14556':2.85,'14736':3.23,'14762':3.31,'14884':3.89,'14945':4.51,'14982':4.85}
I90NWBdic={'14992':0,'14938':0.67,'14837':1.25,'14724':1.77,'14649':2.15,'19878':2.86,'14464':3.00,'14375':3.07,'14348':3.40,'14258':3.73,'14173':4.02}
LakeSBdic={'15159':0,'15191':0.51,'15204':0.78,'15216':1.04,'15253':1.53,'15419':2.56,'15585':3.67,'15776':4.57,'15850':5.24}
LakeNBdic={'15865':0,'15830':0.67,'15630':1.57,'15485':2.68,'15278':3.71,'15227':4.20,'15211':4.46,'15198':4.73,'15168':5.24}
Pedic={'13664':0,'13915':1,'14107':1.75,'14154':1.94,'14224':2.11,'14343':2.51,'14488':3.02,'14760':3.92,'14795':4.05,'14921':4.54,'14961':4.65,'15072':4.91}

crossall=[I90SE,I90NWB,LakeSB,LakeNB,Pe]
crossdicall=[I90SEdic,I90NWBdic,LakeSBdic,LakeNBdic,Pedic]
output=['Veh_VMT_I90SE.dat','Veh_VMT_I90NWB.dat','Veh_VMT_LakeSB.dat','Veh_VMT_LakeNB.dat','Veh_VMT_PE.dat']
inputs=['vehicle_I90SE.dat','vehicle_I90NWB.dat','vehicle_LakeSB.dat','vehicle_LakeNB.dat','vehicle_PE.dat']

path=os.getcwd()
if __name__ == "__main__":
    testid=1
    cross=crossall[testid]
    crossdic=crossdicall[testid]
    nnode=[]
    nodes=[]
    et=[]
    st=[]
    nodevehicle=[]
    etvehicle=[]
    stvehicle=[]

    data2=open(output[testid],'w')
    ## confile=open('C:\Users\Zihan\Desktop\AMS\Simulation\OC1\VehTrajectory.dat','r')
    confile=open(path+'\\'+'VehTrajectory.dat','r')
    data=open(inputs[testid],'w')
    lines=confile.readlines()
    i=5
    outveh=0
    while i<len(lines):
        linearray=lines[i].split( )
        if len(linearray)<1:
            i=i+1
            continue
        if lines[i]==' #############################################\n':
            break
        if len(linearray)==31:
            vehnumber=int(linearray[2])
            nodesnumber=int(linearray[-5])
            starttime=linearray[-13]
            j=int(math.ceil(nodesnumber/10.0))    #whether exceed 10 nodes
            nodevehicle=[]
            for k in range (i+1,i+j+1):
                node=lines[k].rstrip('\n').split( )
                nodevehicle=nodevehicle+node
    ##            print nodevehicle
            count=0
            for p in nodevehicle:
                if int(p) in cross:
    ##                print p
                    count=count+1
                    if count<2:
                        continue
                    else:  # whether go through riverdale
                        nnode.append(str(vehnumber)+'\t'+str(nodesnumber)+'\t'+starttime)
                        nodes.append(nodevehicle)
    ##                    print nodevehicle,i
                        break
    ##                    print nodes
            if count<2:
                i=i+1
                continue
            eti=i+j+1       # go to node exit time line
            etvehicle=[]
            i=eti
            for k in range (i+1,i+1+j):# exit time number
                time=lines[k].rstrip('\n').split( )
                etvehicle=etvehicle+time
            et.append(etvehicle)
    ##        print etvehicle,i
            lti=i+j+1
            i=lti
            sti=i+j+1 # to skip link travel time
            i=sti
            stvehicle=[]
            for k in range (i+1,i+1+j):# acculated stop time number
                time=lines[k].rstrip('\n').split( )
                stvehicle=stvehicle+time
            st.append(stvehicle)
    ##        print stvehicle,i
            i=sti+j+2 
        else:
            i=i+1  
    for p in range (len(st)):
        data.write((nnode[p])+'\n')
        sequence='\t'.join(m for m in nodes[p])+'\n'
        data.write(sequence)
        exitt='\t'.join(m for m in et[p])+'\n'
        data.write(exitt)
        stopt='\t'.join(m for m in st[p])+'\n'
        data.write(stopt)
        data.write('\n')
    data.close
    confile.close
    print ('over')
        
    vehfile=open(inputs[testid],'r')
    data2.write("vid"+'\t'+"miles"+'\t'+'start_time'+'\t'+'traveltime'+'\n')
    lines=vehfile.readlines()
    i=0
    # print d2
    while i<len(lines):
    ##    nodesnumber=int(lines[i])
        vid=lines[i].rstrip('\n').split( )[0]
        starttime=lines[i].rstrip('\n').split( )[-1]
        nodes=lines[i+1].rstrip('\n').split( )  # no calculation of start time
        ettime=lines[i+2].rstrip('\n').split( )  # calculation of start time
        #ettime=[float(time)+starttime for time in ettime_0]
            #sttime=lines[i+3].rstrip('\n').split( )
        ##    sttime=[float(time)+starttime for time in sttime_0]
        distance=[]
        et=[]
        nodesnumber=len(ettime)    # to ignore the last node if the vehicle is still in network
        for p in range(nodesnumber):
            if crossdic.has_key(nodes[p]):
                distance.append(crossdic[nodes[p]])
                et.append(float(ettime[p]))
                    #st.append(sttime[p])
            #vehst.append(sum(st))
        i=i+5
        dis=distance[-1]-distance[0]
        etcorridor=et[-1]-et[0]
        data2.write(vid+'\t'+str(dis)+'\t'+starttime+'\t'+str(etcorridor)+'\n')
    data2.close
    vehfile.close
    print ('over')
        
                
