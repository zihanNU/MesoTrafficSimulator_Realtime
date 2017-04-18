import shutil
import os

filename3='C:\DTAX\\run\\predict\\new_weather.dat'
filename4='C:\DTAX\\run\\predict\\weather.dat'
shutil.copy2(filename3,filename4)
os.remove(filename3) 
print ('weather data updated')    
            
