import random

 


filename='demand.dat'

filename2='demandnew.dat'
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
        if lines[i][0]=='S':
            ttfile2.write('Start Time   ')
            ttfile2.write(str(count*5)+'.0\n')
            count=count+1
        else:   	     
            ttfile2.write(lines[i])
        i=i+1
    ttfile.close()
    ttfile2.close()
 
 





