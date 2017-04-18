
#include <stdio.h>
#include <math.h>

#include "DTAX_LongTermCC_i.h"
#include "../dtax_debug.h"
#include "../dtax_data_convert.h"
#include "../dtax_file_io.h"

/*
  SEE notes in the file ../RTDyna/rtdyna_fortran_callback.cpp
*/



//Added by Kuilin for getting real time data Apr. 8 2005
#include "InternalDataInterfaceForLTCC.h"
//end of add Apr. 8 2005

// added by Tian Hou for riverdale data
#include "RiverdaleLTCC.h"
// end add

//Added by Xiang Fei for getting real time date June 8 2007
const int max_num_obs_link=200;
const int num_obs_intval=18; // modified by Ying 10142013
float density_obs[max_num_obs_link][num_obs_intval];
int G_noofobs;
int G_noofint;
//End add

extern DTAX_LongTermCC_i *the_ltcc_i;
#define THE_LTCC_I	the_ltcc_i

//const int max_num_obs_link=600;
const int max_num_detectors=200*10;
//float RealConc_obs[max_num_obs_link];
//int link_id[max_num_obs_link];
int num_detectors_per_link[max_num_obs_link];
int obs_link_id[max_num_obs_link];
int detectors_id[max_num_detectors];
int links_id[max_num_detectors];
float occupancy[max_num_detectors];
//int G_noofobs;
int link_counter = 0;
int detector_counter = 0;
volatile bool updated;
int previous_local_time;
int data_counter;

// these variables are added for riverdale real-time data
// added by Tian Hou 20131024
bool Riverdale = false;	//flag variable !changed by Zihan

const int max_num_obs_link_riverdale = 200;
const int num_obs_intval_riverdale = 18;
const int max_num_detectors_riverdale=200*10;
int link_counter_riverdale;
int detector_counter_riverdale;
int G_noofobs_riverdale;
int G_noofint_riverdale;
int obs_link_id_riverdale[max_num_obs_link_riverdale];
int detectors_id_riverdale[max_num_detectors_riverdale];
float flow_obs_riverdale[max_num_obs_link_riverdale][num_obs_intval_riverdale];
int previous_local_time_riverdale;
// end add



//Added for getting real time data Xiang Fei June 8 2007
extern "C" void DTAX_FORTRAN_API SEND_INIT_PARA(int *noofobs_in,int *noofint_in)
{
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
}

extern "C" void  DTAX_FORTRAN_API SET_UP_CLIENT()
{
	double Real_Den = 2.7;

	//readDetctorInformation();

	bool m_ReadRealTimeDataFromInternet = true;
	bool m_ReadRealTimeDataFromLocalFile = false;
	fstream fin;
	fin.open("historical_traffic_data_input.txt", ios::in|ios::out);
	if( fin )
		m_ReadRealTimeDataFromLocalFile = true;


	Real_Den=2.7;
	do{
		for (int i_obs_int=0;i_obs_int<G_noofint;i_obs_int++) {
			for(int j_link=0;j_link<G_noofobs;j_link++) {
				density_obs[j_link][i_obs_int]=Real_Den;//+(j_link+(i_obs_int)*G_noofobs)*0.1;			
			}
		}
		//if(m_ReadRealTimeDataFromInternet)ReadRealTimeData();
	   if(m_ReadRealTimeDataFromInternet)ReadRealTimeData_kc();
	   //Modified by Zihan 20170328 for KC project
		if(m_ReadRealTimeDataFromLocalFile)
		{
			int tmp_detector_id;
			float tmp_occupancy;
			int ii = 0;
			int jj = 0;
			for(int i = 0; i < G_noofint; i++){
				for(int j = 0; j < max_num_obs_link; j++ ){
					density_obs[j][i] = 0.0;
				}
				for(int j = 0; j < 909; j++){

					if(!fin.eof()){
						fin >> tmp_detector_id >> tmp_occupancy;
						tmp_occupancy = tmp_occupancy*100;
						for( ii = 0; ii < detector_counter; ii++){
							if( detectors_id[ii] == tmp_detector_id ) break;
						}
						if( ii < detector_counter ){
							for( jj = 0; jj < link_counter; jj++ ){
								if( obs_link_id[jj] == links_id[ii] ) break;
							}
							if( jj < link_counter ){
								density_obs[jj][i] += tmp_occupancy;
							}
						}
					}
					else{
						fin.seekp(ios::beg);
					}

				}
				for( int k = 0; k < link_counter; k++ ){
					density_obs[k][i] /= num_detectors_per_link[k];
				}
			}
		}
		else;
		updated = 1;
		while(updated){
		}
	}while(1);

}

extern "C" void DTAX_FORTRAN_API GET_OBS_LTCC(float *obs_data)
{
	while(!updated){
	}
	for(int i=0;i<G_noofint;i++) {
		for (int j=0;j<G_noofobs;j++) {
		   obs_data[j+i*(G_noofobs)]=density_obs[j][i];
		}
	}
	updated = 0;

	/*int j=CURRENT_TIME_INDEX-1;
	int Timecounter=0;
	try {
		if(G_Detectors.size()==0)
			throw "No real time DATA available";

		int totalElements= G_Detectors[0].m_RealTimeData.size()*G_Detectors.size()-1;
		while (Timecounter<G_Detectors[0].m_RealTimeData.size())
		{   
		   if(j<0) j=G_Detectors[0].m_RealTimeData.size()-1;

 		   for (int i=0;i<G_Detectors.size();i++)
		   {
			  std::string detector_id=G_Detectors[i].m_strDetectorName;
			  long link_id = G_Detectors[i].m_LinkID;	  
			  double flow = G_Detectors[i].m_RealTimeData[j].m_dFlow;		  
			  double speed = G_Detectors[i].m_RealTimeData[j].m_dSpeed;		
			  double density = G_Detectors[i].m_RealTimeData[j].m_dDensity;			

			  int hour=G_Detectors[i].m_RealTimeData[j].m_ObserveTimeStamp.m_Hour;
			  int minute=G_Detectors[i].m_RealTimeData[j].m_ObserveTimeStamp.m_Minute;
			  int second=G_Detectors[i].m_RealTimeData[j].m_ObserveTimeStamp.m_Second;

			  obs_data[totalElements]=G_Detectors[i].m_RealTimeData[j].m_dDensity;
			  totalElements--;
		   }
		   j--;
		   Timecounter++;
		}

	}catch (char *str_error)
	{
		cout<<str_error<<endl;
	}*/


	
}

//*/
//End add

extern "C" void DTAX_FORTRAN_API GET_RTDYNA_ESTIMATE(int *nzones /*0D*/, int *noofarcs /*0D*/, 
											  int *linkprop_size, int *lpcca_fields,
											  int *noofobs,
											  float *xl /*1D*/, float *zdem /*2D*/, 
											  float *ppp_cca /*2D*/, 
											  float *density_avg /*2D*/, int *kkll_cca /*0D*/)
{

	TRACE_LO("LTCC: Callback to GET_RTDYNA_ESTIMATE.\n");

	DTAX::DBKRecord_var lsh;

	lsh = THE_LTCC_I->m_data_man->GetLatestRecord(DTAX::DataManagement::RTDYNA_CCA_LINKPROP);
	DTAX_ASSERT_DEBUG(lsh != NULL);

	int RTDYNA_linkprop_size, RTDYNA_lpcca_fields;

	ReadDBKRecord_L(lsh, 0, (&RTDYNA_linkprop_size), 1);
	DTAX_ASSERT_DEBUG( RTDYNA_linkprop_size == *linkprop_size );
	ReadDBKRecord_L(lsh, 1, (&RTDYNA_lpcca_fields), 1);
	DTAX_ASSERT_DEBUG( RTDYNA_lpcca_fields == *lpcca_fields );
	
	ReadDBKRecord_L(lsh, 2, kkll_cca, 1);

	ReadDBKRecord_F(lsh, 0, xl, *noofarcs);
	ReadDBKRecord_F(lsh, 1, zdem, (*nzones) * (*nzones));
	ReadDBKRecord_F(lsh, 2, ppp_cca, (*linkprop_size) * (*lpcca_fields));
	ReadDBKRecord_F(lsh, 3, density_avg, (*noofarcs) * (*noofobs) );

}

extern "C" void DTAX_FORTRAN_API STORE_DEMAND_CORRECTION(int *numofdests,
														 float *scale_cca /*2D*/, 
														 float *ltccmop /*0D*/)
{
	TRACE_LO("LTCC: Callback to STORE_DEMAND_CORRECTION.\n");
	DTAX::DBKRecord_var lsh;

	lsh = new DTAX::DBKRecord;
	DTAX_CHECK_MEM_ALLOC(lsh);
	
	MakeDBKRecord_F(lsh, 0, scale_cca, (*numofdests) * (*numofdests) );
	MakeDBKRecord_F(lsh, 1, ltccmop, 1);

	lsh->quasi_timestamp = THE_LTCC_I->m_time_now;
	THE_LTCC_I->m_data_man->InsertRecord(DTAX_TIMESTAMP_PTR(lsh), DTAX::DataManagement::CCA_CORRECTION);
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

extern "C" void DTAX_FORTRAN_API GET_OBS_LTCC_RIVERDALE(float *obs_data_riverdale)
{
	for(int i=0; i<G_noofint_riverdale; i++) {
		for (int j=0; j<G_noofobs_riverdale; j++) {
		   obs_data_riverdale[j+i*(G_noofobs_riverdale)] = flow_obs_riverdale[j][i];
		}
	}
}

extern "C" void DTAX_FORTRAN_API STORE_DEMAND_CORRECTION_RIVERDALE(int *numofdests,
														 float *scale_cca /*2D*/, 
														 float *ltccmop_riverdale /*0D*/)
{
	TRACE_LO("LTCC: Callback to STORE_DEMAND_CORRECTION_RIVERDALE.\n");
	DTAX::DBKRecord_var lsh;

	lsh = new DTAX::DBKRecord;
	DTAX_CHECK_MEM_ALLOC(lsh);
	
	MakeDBKRecord_F(lsh, 0, scale_cca, (*numofdests) * (*numofdests) );
	MakeDBKRecord_F(lsh, 1, ltccmop_riverdale, 1);

	lsh->quasi_timestamp = THE_LTCC_I->m_time_now;
	THE_LTCC_I->m_data_man->InsertRecord(DTAX_TIMESTAMP_PTR(lsh), DTAX::DataManagement::CCA_CORRECTION);
}