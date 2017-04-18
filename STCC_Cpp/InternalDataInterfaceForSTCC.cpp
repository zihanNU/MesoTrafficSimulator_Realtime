// InternalDataInterface.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"
//#include "STCC_fortran_call.h"

#include "../dtax_debug.h"
#include "InternalDataInterfaceForSTCC.h"
#include "soapSVC_USCOREC2CSoap12Proxy.h"
#include "SVC_USCOREC2CSoap.nsmap"
#include <fstream>
#include <string>
#include <iomanip>
using namespace std;



//Add by Zihan Hong 20170327 for KC IMRCP project
int ReadRealTimeData_kc()
{   
	int i=0; 
	float speed, volume,density;
   	ifstream fin;
	fin.open("detector.dat");
	while(!fin.eof()){
		fin >> volume>> speed >> density;
        RealSpeed_obs[i]=speed/60.0;
	    RealFlow_obs[i]=volume;
		RealConc_obs[i]=density;
		i++;
	}
	fin.clear();
	fin.close();
	return 0;
}
//End

int ReadRealTimeData()
{
	fstream fs,fs2;
	fs.open ("Freeway_RealData_1.txt",fstream::in | fstream::app); // Add by Ying
	fs2.open ("Freeway_RealData_2.txt",fstream::in | fstream::app); // Add by Ying

	SVC_USCOREC2CSoap12Proxy *service;
	_ns1__OP_USCOREShareTrafficDetectorData *ns1__OP_USCOREShareTrafficDetectorData;
	_ns1__OP_USCOREShareTrafficDetectorDataResponse *ns1__OP_USCOREShareTrafficDetectorDataResponse;
	service = new SVC_USCOREC2CSoap12Proxy;
	ns1__OP_USCOREShareTrafficDetectorDataResponse = new _ns1__OP_USCOREShareTrafficDetectorDataResponse;
	ns1__OP_USCOREShareTrafficDetectorData = new _ns1__OP_USCOREShareTrafficDetectorData;

	if (service->OP_USCOREShareTrafficDetectorData(ns1__OP_USCOREShareTrafficDetectorData, ns1__OP_USCOREShareTrafficDetectorDataResponse) == SOAP_OK)
	{
		int it = 0;
		int j = 0;
		int width = 0;
		int position = -1;
		char s_detector_id[20];
		char s_local_time[20];
		char s_lane_vehicle_count[20];
		char s_occupancy[20];
		char s_lane_vehicle_speed[20];
		string A(service->labbuf);
		position = A.find("local-time");
		A.copy(s_local_time, 6, position+11);
		// When the timestamp for the data has been updated, update the real time data
		if( (atoi(s_local_time)) != previous_local_time ){
			previous_local_time = atoi(s_local_time);
			TRACE_LO("local time: %d\n", previous_local_time);

			//data_counter was designed to deal with different observation intervals of Real time observation and Simulation Observations
			data_counter++;

			for(int i = 0; i < detector_counter; i++){
				vehicle_count[i] = 0;
				occupancy[i] = 0;
				speed[i] = 0;
				for(int ii = 0; ii < 20; ii++){
					s_lane_vehicle_count[ii] = 0;
					s_occupancy[ii] = 0;
					s_lane_vehicle_speed[ii] = 0;
					s_detector_id[ii] = 0;
				}
				itoa(detectors_id[i], s_detector_id, 10);
				position = A.find(s_detector_id);
				position = A.find("lane-vehicle-count",position);
				if ( position != string::npos )
				{
					A.copy(s_lane_vehicle_count, 10, position+19);
					width = 0;
					while( (s_lane_vehicle_count[width] >= 48) && (s_lane_vehicle_count[width] < 57) ){
						width++;
					}
					j = width - 1;
					while( (s_lane_vehicle_count[j] >= 48) && (s_lane_vehicle_count[j] < 57) ){
						vehicle_count[i] = vehicle_count[i] + ( s_lane_vehicle_count[j] - 48 )*pow( 10.0 , width -1 -j );
						j--;
						if(j < 0) break;
					}
					vehicle_count[i] = vehicle_count[i] * 180;//hardwired now
					position = A.find("occupancy",position);
					if ( position != string::npos )
					{
						A.copy(s_occupancy, 10, position+10);
						width = 0;
						while( (s_occupancy[width] >= 48) && (s_occupancy[width] < 57) ){
							width++;
						}
						j = width - 1;
						while( (s_occupancy[j] >= 48) && (s_occupancy[j] < 57) ){
							occupancy[i] = occupancy[i] + ( s_occupancy[j] - 48 )*pow( 10.0 , width - 1 -j );
							j--;
							if(j < 0) break;
						}
						position = A.find("lane-vehicle-speed",position);
						if ( position != string::npos )
						{
							A.copy(s_lane_vehicle_speed, 10, position+19);
							width = 0;
							while( (s_lane_vehicle_speed[width] >= 48) && (s_lane_vehicle_speed[width] < 57) ){
								width++;
							}
							j = width - 1;
							while( (s_lane_vehicle_speed[j] >= 48) && (s_lane_vehicle_speed[j] < 57) ){
								speed[i] = speed[i] + ( s_lane_vehicle_speed[j] - 48 )*pow( 10.0 , width - 1 - j );
								j--;
								if(j < 0) break;
							}
						}
					}
				}
			}
			it = 0;
			if (data_counter == 1){
				for(int i = 0; i < link_counter; i++){
					RealConc_obs[i] = 0;
					RealFlow_obs[i] = 0;
					RealSpeed_obs[i] = 0;
					for(int jj = 0; jj < num_detectors_per_link[i]; jj++){
						//if(detector_links_id[it] = 1049){
						//fs << setw(10) << previous_local_time << setw(10) << detector_links_id[it] << setw(5) << occupancy[it] << setw(5) << vehicle_count[it] << setw(5) << speed[it] << endl; // modified by Ying 10162013
						//}
						//if(detector_links_id[it] = 1058){
						//fs2 << setw(10) << previous_local_time << setw(10) << detector_links_id[it] << setw(5) << occupancy[it] << setw(5) << vehicle_count[it] << setw(5) << speed[it] << endl; // modified by Ying 10162013
						//}
						RealConc_obs[i] += occupancy[it];
						RealFlow_obs[i] += vehicle_count[it];
						RealSpeed_obs[i] += speed[it];
						it++;
						
					}
					
					RealConc_obs[i] /= num_detectors_per_link[i];
					RealSpeed_obs[i] /= num_detectors_per_link[i];

					if(detector_links_id[it-1] = 1049){
						fs << setw(10) << detector_links_id[it-1]<< setw(10) << RealFlow_obs[i]  << setw(10) << RealSpeed_obs[i] << endl; // modified by Ying 10162013
						}
					if(detector_links_id[it-1] = 1058){
						fs2 << setw(10) << detector_links_id[it-1]<< setw(10) << RealFlow_obs[i]  << setw(10) << RealSpeed_obs[i] << endl; // modified by Ying 10162013
						}
				}
				data_counter --;
				//updated = 1;
			}
		}

	}

	delete service;
	delete ns1__OP_USCOREShareTrafficDetectorData;;
	delete ns1__OP_USCOREShareTrafficDetectorDataResponse;;
	return 0;
	fs.close();
	fs2.close();
}


