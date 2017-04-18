#include "DTAX_ShortTermCC_i.h"
#include "../dtax_os.h"
#include "../dtax_debug.h"
#include "../dtax_data_convert.h"
#include "../dtax_file_io.h"
//#include "STCC_fortran_call.h"
//Added for getting real time data May 26 2005

#include <fstream>
using namespace std;
#include "InternalDataInterfaceForSTCC.h"

//End add
/*
  SEE notes in the file ../RTDyna/rtdyna_fortran_callback.cpp
*/

// added by Tian Hou for riverdale data
#include "RiverdaleSTCC.h"
// end add

//Added by Xiang Fei for getting real time date June 8 2007
const int max_num_obs_link=200;
const int max_num_detectors=200*10;
float RealFlow_obs[max_num_obs_link];
float RealConc_obs[max_num_obs_link];
float RealSpeed_obs[max_num_obs_link];
int observation_links_id[max_num_obs_link];//the positions of observation links in all links
int num_detectors_per_link[max_num_obs_link];
int obs_link_id[max_num_obs_link];
int detectors_id[max_num_detectors];
int detector_links_id[max_num_detectors];
float vehicle_count[max_num_detectors];
float occupancy[max_num_detectors];
float speed[max_num_detectors];
int G_noofobs;
int link_counter;
int detector_counter;
volatile bool updated;
int previous_local_time;
int data_counter;
//End add


// these variables are added for riverdale real-time data
// added by Tian Hou 20131024
bool Riverdale = false;	//flag variable  !Changed by Zihan 20170328

const int max_num_obs_link_riverdale=200;
const int max_num_detectors_riverdale=200*10;
int link_counter_riverdale;
int detector_counter_riverdale;
int G_noofobs_riverdale;
int obs_link_id_riverdale[max_num_obs_link_riverdale];
int detectors_id_riverdale[max_num_detectors_riverdale];
float RealFlow_obs_riverdale[max_num_obs_link];
float RealSpeed_obs_riverdale[max_num_obs_link];
int previous_local_time_riverdale;
// end add


extern DTAX_ShortTermCC_i *the_stcc_i;
#define THE_STCC_I	the_stcc_i

// the GETSTCCARRAY subroutine retrieves a variety of data
// (see parameter list) from the data broker and return
// their values to the calling function (STCC) 

void store_updated_realtime_data()
{
	TRACE_LO("STCC: Callback to STORE_UPDATED_REALTIME_DATA.\n");
	DTAX::DBKRecord_var lsh;
	
	lsh = new DTAX::DBKRecord;
	DTAX_CHECK_MEM_ALLOC(lsh);

	int tmp_link_counter = link_counter;
	MakeDBKRecord_L(lsh, 0, (&previous_local_time), 1);
	MakeDBKRecord_L(lsh, 1, (&tmp_link_counter), 1);
	MakeDBKRecord_L(lsh, 2, observation_links_id, tmp_link_counter);
	MakeDBKRecord_F(lsh, 0, RealFlow_obs, tmp_link_counter);
	MakeDBKRecord_F(lsh, 1, RealConc_obs, tmp_link_counter);
	MakeDBKRecord_F(lsh, 2, RealSpeed_obs, tmp_link_counter);
	
	lsh->quasi_timestamp = THE_STCC_I->m_time_now;
	THE_STCC_I->m_data_man->InsertRecord(DTAX_TIMESTAMP_PTR(lsh), DTAX::DataManagement::GUISTLINKDATA);
}

//Added by Xiang Fei for getting real time date June 8 2007
extern "C" void DTAX_FORTRAN_API SEND_INIT_PARA(int *noofobs_in)
{
	G_noofobs = *noofobs_in;
	//ReadDetectorInfo();
	//Read detector_mapping_to_link and make relevant change to referencetable.txt, they are not consistent
	int detector_id, link_id, up_node_id, down_node_id;
	double temp1, temp2;
	int pre_link_id = 0;
	link_id = 0;
	ifstream fin;
	fstream fs;
	link_counter = 0;
	detector_counter = 0;
	fin.open("detector_mapping_to_link.txt");
	fs.open("referencetable.txt");
	fs.seekg(0, ios::end);
	int m = fs.tellg();		// m will be equal to 86458
	for(int i = 1; (i*74-2) < m; i++){
		fs.seekp(i*74-3);
		fs << "  0";
	}
	int width_of_link_id;
	int tmp_link_counter;
	int tmp_num_detectors = 0;
	while(!fin.eof()){
		fin >> detector_id >> temp1 >> temp2 >> link_id >> up_node_id >> down_node_id;
		detectors_id[detector_counter] = detector_id;
		detector_links_id[detector_counter] = link_id;
		if( link_id != pre_link_id ){
			if(link_counter > 0){
				num_detectors_per_link[link_counter - 1] = tmp_num_detectors;
				obs_link_id[link_counter - 1] = link_id;
			}
			observation_links_id[link_counter] = link_id;
			tmp_num_detectors = 1;
			pre_link_id = link_id;
			link_counter++;
			tmp_link_counter = link_counter;
			width_of_link_id = 1;
			while(tmp_link_counter/10 >= 1){
				width_of_link_id++;
				tmp_link_counter = tmp_link_counter/10;
			}
			if( (link_id*74 - width_of_link_id) < m ){
				fs.seekp(link_id*74 - width_of_link_id);
				fs << link_counter;
			}
		}
		else{
			tmp_num_detectors++;
		}
		detector_counter++;
	}
	num_detectors_per_link[link_counter - 1] = tmp_num_detectors;
	obs_link_id[link_counter - 1] = link_id;
	fs.close();
	fin.close();
}

extern "C" void  DTAX_FORTRAN_API SET_UP_CLIENT()
{
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
	previous_local_time_riverdale = 0;
	// end
	//******************************************************************************

	
	double Real_Den = 2.7;

	//readDetctorInformation();
	previous_local_time = 0;
	data_counter = 0;

	bool m_ReadRealTimeDataFromInternet = true;
	bool m_ReadRealTimeDataFromLocalFile = false;
	fstream fin;
	fin.open("historical_traffic_data_input.txt", ios::in|ios::out);
	if( fin )
		m_ReadRealTimeDataFromLocalFile = true;


	do {
		// this part is added for riverdale (SLC TrEPS project)
		// added by Tian Hou 20131025
		if (Riverdale)
			ReadRealTimeData_riverdale(m_pConnection, m_pRecordset);		
		//end


   	   for(int j_link=0;j_link<G_noofobs;j_link++) {
	      RealConc_obs[j_link]=Real_Den;//+(j_link)*0.1;			
	   }
		//if(m_ReadRealTimeDataFromInternet)ReadRealTimeData();
	   if(m_ReadRealTimeDataFromInternet)ReadRealTimeData_kc();
	   //Modified by Zihan 20170328
 		if(m_ReadRealTimeDataFromLocalFile)
		{
			int tmp_detector_id;
			float tmp_occupancy;
			int ii = 0;
			int jj = 0;
			for(int j = 0; j < max_num_obs_link; j++ ){
				RealConc_obs[j] = 0.0;
			}
			//for(int j = 0; j < 908; j++){ // modified by Ying 10/09/2013
			for(int j = 0; j < 38; j++){ 
				if(!fin.eof()){
					fin >> tmp_detector_id >> tmp_occupancy;
					tmp_occupancy = tmp_occupancy*100;
					for( ii = 0; ii < detector_counter; ii++){
						if( detectors_id[ii] == tmp_detector_id ) break;
					}
					if( ii < detector_counter ){
						for( jj = 0; jj < link_counter; jj++ ){
							if( obs_link_id[jj] == detector_links_id[ii] ) break;
						}
						if( jj < link_counter ){
							RealConc_obs[jj] += tmp_occupancy;
						}
					}
				}
				else{
					fin.seekp(ios::beg);
				}

			}
			for( int k = 0; k < link_counter; k++ ){
				RealConc_obs[k] /= num_detectors_per_link[k];
			}
		}
		else;
		updated = 1;
		store_updated_realtime_data();
		while(updated){
		}
	}while (1);

}


extern "C" void DTAX_FORTRAN_API GET_OBS_STCC(float *obs_data)
{	
	while(!updated){
	}
	for (int j=0;j<G_noofobs;j++) {
		obs_data[j]=RealConc_obs[j];
	}
	updated = 0;
    //for (int i=0;i<G_Detectors.size();i++)
	//{
//Added more code to determine the detector link ID to the corresponding observation data
//....................

	//	obs_data[i]=G_Detectors[i].m_RealTimeData[0].m_dDensity;
	//}
	//TRACE_LO
}
//End add

extern "C" void DTAX_FORTRAN_API GET_FLOW_PROP(int *noofarcs, float *c /*1D OUT*/, 
											   float *cmax /*1D OUT*/, 
											   float *p /*1D OUT*/, 
											   float *vmax /*1D OUT*/, 
											   float *v0 /*1D OUT*/,
											   float *vUpBound /*1D OUT*/,
											   float *v /*1D OUT*/,
										       int *LinkIndexOrg /*1D OUT*/)
{
	TRACE_LO("STCC: Callback to GET_FLOW_PROP.\n");
	DTAX::DBKRecord_var lsh;

	int numpast = THE_STCC_I->m_data_man->GetRecordCount(DTAX::DataManagement::RTDYNA_FLOW_PROP);
//Modified by Xiao and Xiang, June 27 2005
//	numpast = (numpast > 6) ? 6:numpast;
	numpast = (numpast > 51) ? 51:numpast;
//End
	lsh = THE_STCC_I->m_data_man->GetRecordAverage(DTAX::DataManagement::RTDYNA_FLOW_PROP, numpast);

	ReadDBKRecord_L(lsh, 0, LinkIndexOrg, *noofarcs);

	ReadDBKRecord_F(lsh, 0, c, *noofarcs);
	ReadDBKRecord_F(lsh, 1, cmax, *noofarcs);
	ReadDBKRecord_F(lsh, 2, p, *noofarcs);
	ReadDBKRecord_F(lsh, 3, vmax, *noofarcs);
	ReadDBKRecord_F(lsh, 4, v0, *noofarcs);
	ReadDBKRecord_F(lsh, 5, vUpBound, *noofarcs);
	ReadDBKRecord_F(lsh, 6, v, *noofarcs);

}

// the STOREUPDATEDSPEED subroutine stores the updated
// speed from the calling function (STCC) to the data broker. 

extern "C" void DTAX_FORTRAN_API STORE_UPDATED_SPEED(int *noofarcs,
													 float *v /*1D*/, 
													 float *stccmop /*0D*/)
{
	TRACE_LO("STCC: Callback to STORE_UPDATED_SPEED.\n");
	DTAX::DBKRecord_var lsh;
	
	lsh = new DTAX::DBKRecord;
	DTAX_CHECK_MEM_ALLOC(lsh);

	MakeDBKRecord_F(lsh, 0, v, *noofarcs);
	MakeDBKRecord_F(lsh, 1, stccmop, 1);
	
	lsh->quasi_timestamp = THE_STCC_I->m_time_now;
	THE_STCC_I->m_data_man->InsertRecord(DTAX_TIMESTAMP_PTR(lsh), DTAX::DataManagement::CCB_UPDATED_SPEED);
}


//Added by Xiao and Xiang June 30 2005 (retriving the latest time interval's speed)
extern "C" void DTAX_FORTRAN_API GET_LATEST_SPEED(int *noofarcs, float *v /*1D OUT*/)
{
	TRACE_LO("STCC: Callback to GET_LATEST_SPEED.\n");
	DTAX::DBKRecord_var lsh;

	lsh = THE_STCC_I->m_data_man->GetLatestRecord(DTAX::DataManagement::RTDYNA_FLOW_PROP);

/*	ReadDBKRecord_L(lsh, 0, LinkIndexOrg, *noofarcs);

	ReadDBKRecord_F(lsh, 0, c, *noofarcs);
	ReadDBKRecord_F(lsh, 1, cmax, *noofarcs);
	ReadDBKRecord_F(lsh, 2, p, *noofarcs);
	ReadDBKRecord_F(lsh, 3, vmax, *noofarcs);
	ReadDBKRecord_F(lsh, 4, v0, *noofarcs);
	ReadDBKRecord_F(lsh, 5, vUpBound, *noofarcs); */
	ReadDBKRecord_F(lsh, 6, v, *noofarcs);
}
//End add

/***************************************************************************************
the following functions are used for Riverdale real-time data, for SLC TrEPS project only
***************************************************************************************/

extern "C" void DTAX_FORTRAN_API SEND_INIT_PARA_RIVERDALE(int *noofobs_in_riverdale)
{
	G_noofobs_riverdale = *noofobs_in_riverdale;
	//Read detector_mapping_to_link_riverdale.txt
	int detector_id, link_id, up_node_id, down_node_id;
	double temp1, temp2;
	link_id = 0;
	ifstream fin;
	fin.open("detector_mapping_to_link_riverdale.txt");
	link_counter_riverdale = 0;
	detector_counter_riverdale = 0;

	while(!fin.eof()){
		fin >> detector_id >> temp1 >> temp2 >> link_id >> up_node_id >> down_node_id;
		detectors_id_riverdale[detector_counter_riverdale] = detector_id;
		obs_link_id_riverdale[link_counter_riverdale] = link_id;
		link_counter_riverdale++;
		detector_counter_riverdale++;
	}
	fin.clear();
	fin.close();
}

extern "C" void DTAX_FORTRAN_API GET_OBS_STCC_RIVERDALE(float *obs_data_riverdale)
{
	for (int j=0;j<G_noofobs_riverdale;j++) {
		obs_data_riverdale[j]=RealSpeed_obs_riverdale[j];
	}
}

extern "C" void DTAX_FORTRAN_API STORE_UPDATED_SPEED_RIVERDALE(int *noofarcs, float *v, float *stccmop_riverdale)
{
	TRACE_LO("STCC: Callback to STORE_UPDATED_SPEED_RIVERDALE.\n");
	DTAX::DBKRecord_var lsh;
	
	lsh = new DTAX::DBKRecord;
	DTAX_CHECK_MEM_ALLOC(lsh);

	MakeDBKRecord_F(lsh, 0, v, *noofarcs);
	MakeDBKRecord_F(lsh, 1, stccmop_riverdale, 1);
	
	lsh->quasi_timestamp = THE_STCC_I->m_time_now;
	THE_STCC_I->m_data_man->InsertRecord(DTAX_TIMESTAMP_PTR(lsh), DTAX::DataManagement::CCB_UPDATED_SPEED);

}