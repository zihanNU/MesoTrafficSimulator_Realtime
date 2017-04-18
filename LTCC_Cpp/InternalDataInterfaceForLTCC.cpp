// InternalDataInterface.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"

#include "../dtax_debug.h"
#include "InternalDataInterfaceForLTCC.h"
#include "soapSVC_USCOREC2CSoap12Proxy.h"
#include "SVC_USCOREC2CSoap.nsmap"
#include <fstream>
#include <string>

#include "RiverdaleLTCC.h"

using namespace std;


void readDetctorInformation ()
{

}
//Add by Zihan Hong 20170327 for KC IMRCP project
int ReadRealTimeData_kc()
{   
	int i=0; 
	float speed, volume,density;
   	ifstream fin;
	fin.open("detector.dat");
	while(!fin.eof()){
		fin >> volume>> speed >> density;
		density_obs[i][0]=density;
		i++;
	}
	fin.clear();
	fin.close();
	return 0;
}
//End

int ReadRealTimeData()
{
	int time_interval_counter = 0;

	
	SVC_USCOREC2CSoap12Proxy *service;
	service = new SVC_USCOREC2CSoap12Proxy;
	_ns1__OP_USCOREShareTrafficDetectorData *ns1__OP_USCOREShareTrafficDetectorData;
	_ns1__OP_USCOREShareTrafficDetectorDataResponse *ns1__OP_USCOREShareTrafficDetectorDataResponse;
	ns1__OP_USCOREShareTrafficDetectorDataResponse = new _ns1__OP_USCOREShareTrafficDetectorDataResponse;
	ns1__OP_USCOREShareTrafficDetectorData = new _ns1__OP_USCOREShareTrafficDetectorData;


	//******************************************************************************
	//added by Tian Hou for riverdale
	// define connection pointer and dataset pointer
	ADODB::_ConnectionPtr m_pConnection;
	ADODB::_RecordsetPtr m_pRecordset; 
	// initialization
	CoInitialize(NULL);
	m_pConnection.CreateInstance("ADODB.Connection");
	if (Riverdale)
	{
		int vpn_connect = system("ping -n 2 -w 500 168.178.127.79 > nul");	//ping server to check vpn connection
		if (vpn_connect == 0)
		{
			try{
				// establish connection
				m_pConnection->Open("Provider=SQLOLEDB; Server=168.178.127.79; uid=datareader; pwd=seemenow;", "", "", ADODB::adModeUnknown);
				TRACE_LO("Connect to Riverdale database successfully...\n\n");
			}
			catch(_com_error e){
				TRACE_LO("\n***************************************\n");
				TRACE_LO("Connect to Riverdale database failed...\n");
				TRACE_LO("***************************************\n\n");
			}
		}
		else
		{
			TRACE_LO("\n***************************************************************************\n");
			TRACE_LO("Cannot connect to 168.178.127.79, please check internet and vpn connection!\n");
			TRACE_LO("***************************************************************************\n\n");
		}
	}
	// end
	//******************************************************************************


	while( time_interval_counter < G_noofint )
	{
		if (service->OP_USCOREShareTrafficDetectorData(ns1__OP_USCOREShareTrafficDetectorData, ns1__OP_USCOREShareTrafficDetectorDataResponse) == SOAP_OK)
		{
			int it = 0;
			int j = 0;
			int width = 0;
			int position = -1;
			char s_detector_id[20];
			char s_local_time[10];
			char s_occupancy[10];
			string A(service->labbuf);
			position = A.find("local-time");
			A.copy(s_local_time, 6, position+11);
			if( (atoi(s_local_time)) != previous_local_time ){
				previous_local_time = atoi(s_local_time);
				TRACE_LO("local time: %d\n", previous_local_time);

				// read riverdale real-time data, added by Tian Hou 20131105
				if (Riverdale){
					if (m_pConnection->State)
						ReadRealTimeData_riverdale(m_pConnection, m_pRecordset, time_interval_counter);
					else{
						TRACE_LO("\n***************************************\n");
						TRACE_LO("Connect to Riverdale database failed...\n");
						TRACE_LO("***************************************\n\n");
					}
				}
				// end add

				data_counter++;
				for(int i = 0; i < detector_counter; i++){
					for(int ii = 0; ii < 20; ii++){
						s_detector_id[ii] = 0;
					}
					occupancy[i] = 0.0;
					itoa(detectors_id[i], s_detector_id, 10);
					position = A.find(s_detector_id);
					position = A.find("occupancy",position);
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
				}
				it = 0;
				if (data_counter == 1){
					for(int i = 0; i < link_counter; i++){
						density_obs[i][time_interval_counter] = 0;
						for(int j = 0; j < num_detectors_per_link[i]; j++){
							density_obs[i][time_interval_counter] += occupancy[it];
							it++;
						}
						density_obs[i][time_interval_counter] /= num_detectors_per_link[i];
					}
					data_counter--;
				}
				time_interval_counter++;
				//updated = 1;
			}
		}
	}

	delete service;
	delete ns1__OP_USCOREShareTrafficDetectorDataResponse;
	delete ns1__OP_USCOREShareTrafficDetectorData;

	// added by Tian Hou for riverdale
	if (m_pConnection->State)
		m_pConnection->Close();
	// end add

	return 0;
}

