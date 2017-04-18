
#include <stdio.h>
#include <math.h>
#include <iostream> 
#include <afxwin.h> 

#include "DTAX_ODEstimate_i.h"
#include "../dtax_os.h"
#include "../dtax_debug.h"
#include "../dtax_data_convert.h"
#include "../dtax_file_io.h"

using namespace std;

/*
  SEE notes in the file ../RTDyna/rtdyna_fortran_callback.cpp
*/

//Added by Kuilin for getting real time data May 26 2005
#include "InternalDataInterface.h"

// added by Tian Hou for riverdale data
#include "RiverdaleODE.h"
// end add

//Added by Xiang Fei for getting real time date June 8 2007
const int max_num_obs_link=200;
float RealConc_obs[max_num_obs_link];
int G_noofobs;
//End add

extern DTAX_ODEstimate_i *the_ode_i;
#define THE_ODE_I	the_ode_i

//added by Tian Hou 20130121 for SLC TrEPS ODE modification
const int max_num_detectors = 200*10;
const int num_obs_intval = 50;
int num_detectors_per_link[max_num_obs_link];
int obs_link_id[max_num_obs_link];
int detectors_id[max_num_detectors];
int links_id[max_num_detectors];
int vehicle_count[max_num_detectors];
float RealFlow_obs[max_num_obs_link];
int G_noofint;
volatile bool updated;
int link_counter = 0;
int detector_counter = 0;
int previous_local_time;
int data_counter;
//end add by Tian Hou 20130121 for SLC TrEPS ODE modification

// these variables are added for riverdale real-time data
// added by Tian Hou 20140203
bool Riverdale = false;		//flag variable !Changed by Zihan
const int max_num_obs_link_riverdale = 200;
const int num_obs_intval_riverdale = 18;
const int max_num_detectors_riverdale = 200*10;
int link_counter_riverdale;
int detector_counter_riverdale;
int G_noofobs_riverdale;
int G_noofint_riverdale;
int obs_link_id_riverdale[max_num_obs_link_riverdale];
int detectors_id_riverdale[max_num_detectors_riverdale];
float flow_obs_riverdale[max_num_obs_link_riverdale];
int previous_local_time_riverdale;
// end add


//Added by Xiang Fei for getting real time date June 8 2007
//extern "C" void DTAX_FORTRAN_API SEND_INIT_PARA_TESTSTCC(int *noofobs_in, int *noofint_in)
//{
//    G_noofobs = *noofobs_in;
//	G_noofint = *noofint_in;
//	//ReadDetectorInfo();
//	//Read detector_mapping_to_link and make relevant change to referencetable.txt, they are not consistent
//	int detector_id, link_id, up_node_id, down_node_id;
//	double temp1, temp2;
//	int pre_link_id = 0;
//	link_id = 0;
//	ifstream fin;
//	fstream fs;
//	link_counter = 0;
//	detector_counter = 0;
//	fin.open("detector_mapping_to_link.txt");
//	fs.open("referencetable.txt");
//	fs.seekg(0, ios::end);
//	int m = fs.tellg();		// m will be equal to 86458
//	for(int i = 1; (i*74-2) < m; i++){
//		fs.seekp(i*74-3);
//		fs << "  0";
//	}
//	int width_of_link_id;
//	int tmp_link_counter;
//	int tmp_num_detectors = 0;
//	while(!fin.eof()){
//		fin >> detector_id >> temp1 >> temp2 >> link_id >> up_node_id >> down_node_id;
//		detectors_id[detector_counter] = detector_id;
//		detector_links_id[detector_counter] = link_id;
//		if( link_id != pre_link_id ){
//			if(link_counter > 0){
//				num_detectors_per_link[link_counter - 1] = tmp_num_detectors;
//				obs_link_id[link_counter - 1] = link_id;
//			}
//			observation_links_id[link_counter] = link_id;
//			tmp_num_detectors = 1;
//			pre_link_id = link_id;
//			link_counter++;
//			tmp_link_counter = link_counter;
//			width_of_link_id = 1;
//			while(tmp_link_counter/10 >= 1){
//				width_of_link_id++;
//				tmp_link_counter = tmp_link_counter/10;
//			}
//			if( (link_id*74 - width_of_link_id) < m ){
//				fs.seekp(link_id*74 - width_of_link_id);
//				fs << link_counter;
//			}
//		}
//		else{
//			tmp_num_detectors++;
//		}
//		detector_counter++;
//	}
//	num_detectors_per_link[link_counter - 1] = tmp_num_detectors;
//	obs_link_id[link_counter - 1] = link_id;
//	fs.close();
//	fin.close();
//}


extern "C" void DTAX_FORTRAN_API SEND_INIT_PARA(int *noofobs_in, int *noofint_in)
{
	//added by Tian Hou 20130121 for SLC TrEPS ODE modification
	//it is actually copied from Sihan Cheng's LTCC code
	G_noofobs = *noofobs_in;
	G_noofint = *noofint_in;
	int detector_id =0;
	int link_id = 0;
	int up_node_id = 0;
	int down_node_id = 0;
	int temp1 = 0;
	int temp2 = 0;
	int pre_link_id = 0;
	link_id = 0;
	fstream fin;
	fstream fs;
	link_counter = 0;
	detector_counter = 0;
	fin.open("detector_mapping_to_link.txt", ios::in|ios::out);
	fs.open("referencetable.txt", ios::in|ios::out);
	fs.seekg(0, ios::end);
	int m = fs.tellg();
	for(int i = 1; (i*74-2) < m; i++){
		fs.seekp(i*74-3);
		fs << "  0";
	}
	int width_of_link_id = 0;
	int tmp_link_counter = 0;
	int tmp_num_detectors = 0;
	while(!fin.eof()){
		fin >> detector_id >> temp1 >> temp2 >> link_id >> up_node_id >> down_node_id;
		detectors_id[detector_counter] = detector_id;
		links_id[detector_counter] = link_id;
		if( link_id != pre_link_id ){
			if(link_counter > 0){
				num_detectors_per_link[link_counter - 1] = tmp_num_detectors;
				obs_link_id[link_counter - 1] = link_id;
			}
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
	//end add by Tian Hou 20130121 for SLC TrEPS ODE modification

}

extern "C" void  DTAX_FORTRAN_API SET_UP_CLIENT()
{
	//added by Tian Hou 20130121 for SLC TrEPS ODE modification
	//it is actually copied from Sihan Cheng's LTCC code
	double Real_Den = 2.7;
	bool m_ReadRealTimeDataFromInternet = true;
	bool m_ReadRealTimeDataFromLocalFile = false;
	fstream fin;
	fin.open("historical_traffic_data_input.txt", ios::in|ios::out);
	if( fin )
		m_ReadRealTimeDataFromLocalFile = true;

	do{
	   //if(m_ReadRealTimeDataFromInternet)ReadRealTimeData();
	   if(m_ReadRealTimeDataFromInternet)ReadRealTimeData_kc();
	   //Modified by Zihan 20170328 for KC project
		updated = 1;
		while(updated){
		}
	}while(1);
	//end add by Tian Hou

}


//Added for getting XML data  Apr. 8 2005
//*
extern "C" void DTAX_FORTRAN_API GET_OBS_ODE(float *obs_data)
{	
	//added by Tian Hou 20130121 for SLC TrEPS ODE modification
	while(!updated){
	}
	for (int i=0;i<G_noofobs;i++) {
	   obs_data[i] = RealFlow_obs[i];
	}
	updated = 0;
	//end add by Tian Hou 20130121 for SLC TrEPS ODE modification

}
//*/
//End add

extern "C" void DTAX_FORTRAN_API GET_LINKPROP1(int *lp_count, int *field_count)
{
	
	TRACE_LO("ODE: Callback to GET_LINKPROP1.\n");
	DTAX::DBKRecord_var lsh;

	lsh = THE_ODE_I->m_data_man->GetLatestRecord(DTAX::DataManagement::PDYNA_LINKPROP);
	DTAX_ASSERT_DEBUG(lsh != NULL);

	int PDYNA_lp_count, PDYNA_field_count;

	ReadDBKRecord_L(lsh, 0, (&PDYNA_lp_count), 1);
	ReadDBKRecord_L(lsh, 1, (&PDYNA_field_count), 1);

	(*lp_count)=PDYNA_lp_count;
	//	DTAX_ASSERT_DEBUG( *lp_count == PDYNA_lp_count );
	DTAX_ASSERT_DEBUG( *field_count == PDYNA_field_count );

//	ReadDBKRecord_F(lsh, 0, proportion_ode, (*lp_count) * (*field_count));

}


extern "C" void DTAX_FORTRAN_API GET_LINKPROP2(float *proportion_ode /*2D OUT*/) 
{
	
	TRACE_LO("ODE: Callback to GET_LINKPROP2.\n");
	DTAX::DBKRecord_var lsh;

	lsh = THE_ODE_I->m_data_man->GetLatestRecord(DTAX::DataManagement::PDYNA_LINKPROP);
	DTAX_ASSERT_DEBUG(lsh != NULL);

	int PDYNA_lp_count, PDYNA_field_count;
	ReadDBKRecord_L(lsh, 0, (&PDYNA_lp_count), 1);
	ReadDBKRecord_L(lsh, 1, (&PDYNA_field_count), 1);
//	DTAX_ASSERT_DEBUG( *lp_count == PDYNA_lp_count );
//	DTAX_ASSERT_DEBUG( *field_count == PDYNA_field_count );

	ReadDBKRecord_F(lsh, 0, proportion_ode, (PDYNA_lp_count) * (PDYNA_field_count));

}


extern "C" void DTAX_FORTRAN_API STORE_PREDICTED_COEFF(float *time_start,
													   int *nod, int *ncoef,
													   float *bbk2 /*?D*/) 
//													   float *bbk3 /*?D*/, 
//													   float *bbk4 /*?D*/) 
//													   float *ppk2 /*?D*/)
{
	TRACE_LO("ODE: Callback to STORE_PREDICTED_COEFF.\n");
	DTAX::DBKRecord_var lsh;

//	int ppk2size = ((*nod)*(*ncoef))*((*nod)*(*ncoef));

	lsh = new DTAX::DBKRecord;
	DTAX_CHECK_MEM_ALLOC(lsh);

	MakeDBKRecord_L(lsh, 0, nod, 1);
	MakeDBKRecord_L(lsh, 1, ncoef, 1);
	MakeDBKRecord_F(lsh, 0, time_start, 1);
	MakeDBKRecord_F(lsh, 1, bbk2, (*nod) * (*ncoef));
//	MakeDBKRecord_F(lsh, 2, bbk3, (*nod) * (*ncoef));
//	MakeDBKRecord_F(lsh, 3, bbk4, (*nod) * (*ncoef));
//	MakeDBKRecord_F(lsh, 4, ppk2, ppk2size);
	lsh->quasi_timestamp = THE_ODE_I->m_time_now;
	THE_ODE_I->m_data_man->InsertRecord(DTAX_TIMESTAMP_PTR(lsh), DTAX::DataManagement::ODE_PREDICTED_COEFF);
}


extern "C" void DTAX_FORTRAN_API GET_INITIAL_GUESS(int *nod, int *ncoef,
												   float *bbk2 /*1D OUT*/)
//													 float *ppk2 /*2D OUT*/)
{
	TRACE_LO("ODE: Callback to GET_INITIAL_GUESS.\n");
	DTAX::DBKRecord_var lsh;

	lsh = THE_ODE_I->m_data_man->GetLatestRecord(DTAX::DataManagement::ODE_PREDICTED_COEFF);
	DTAX_ASSERT_DEBUG(lsh != NULL);

	// retrieve data
	ReadDBKRecord_F(lsh, 1, bbk2, (*nod)*(*ncoef));
}

/***************************************************************************************
the following functions are used for Riverdale real-time data, for SLC TrEPS project only
***************************************************************************************/

extern "C" void DTAX_FORTRAN_API SEND_INIT_PARA_RIVERDALE(int *noofobs_in_riverdale,int *noofint_in_riverdale)
{
	G_noofobs_riverdale = *noofobs_in_riverdale;
	G_noofint_riverdale = *noofint_in_riverdale;
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

extern "C" void DTAX_FORTRAN_API GET_OBS_ODE_RIVERDALE(float *obs_data_riverdale)
{
	for (int i=0; i<G_noofobs_riverdale; i++) {
	   obs_data_riverdale[i] = flow_obs_riverdale[i];
	}
}