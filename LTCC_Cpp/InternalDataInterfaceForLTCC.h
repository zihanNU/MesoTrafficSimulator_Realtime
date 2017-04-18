#pragma once
//
//#include <windows.h>
#include <conio.h>

//STL
#include <vector>
#include <list>
#include <algorithm>
#include <math.h>
#include <string>
#include <iostream>
#include <wchar.h>
#include <time.h>
#include <stdlib.h>
#include <STDIO.H>
#include <map>
#include <fstream>
#include <string>
#include "TCHAR.h"

//MSXML
#import "msxml6.dll"

//global variable
static _TCHAR   szTemp[_MAX_PATH];         // multipurpose buffer on stack
static DWORD    dwLen;   

////////////////////////////////////////////////////////////////////////////
//classes
class CDateTime
{
public:
	int m_Year;
	int m_Month;
	int m_Day;
	int m_Hour;
	int m_Minute;
	int m_Second;
};

class CDetectorData
{
public:
	CDateTime m_ObserveTimeStamp;
	long m_lConvertTimeStamp;
	int m_iTimeIntervalLength;
	long m_lTimeStampDate;
	long m_lTimeStampTime;
	int m_iTimeIndex;
	long m_lAbsoluteTimeIndex;
	double m_dFlow;
	double m_dOccupancy;
	double m_dSpeed;
	double m_dQueueLength;
	double m_dDensity;
	int m_laneNumber;
};

class CDetector
{
public:
	int m_nID;
	std::string m_strDetectorName;
    int m_LinkID;
	int m_FromID;
	int m_ToID;
	float m_obsData;

	std::vector<CDetectorData> m_RealTimeData;
};

//////////////////////////////////////////////////////////////////////////
extern long NO_OF_TIMEINTERVALS_IN_XML;
extern int NO_OF_DETECTORS;
extern std::vector<CDetector> G_Detectors;
extern char WORKING_FOLDER[_MAX_PATH];
extern char URL_XML[_MAX_PATH];
extern float TIME_INTERVAL_LENGTH;
extern long NO_OF_TIMEINTERVALS;


extern int G_noofint;
extern int G_hour,G_minute,G_second;
extern float density_obs[200][18];
extern long CURRENT_TIME_INDEX;
extern const int max_num_obs_link;
extern const int max_num_detectors;
extern volatile bool updated;
extern int link_counter;
extern int detector_counter;
extern int data_counter;
extern int previous_local_time;
extern int detectors_id[2000];
extern int links_id[2000];
extern float occupancy[2000];
extern int num_detectors_per_link[200];
extern int obs_link_nums[200];

//Functions
extern void dprintf( char * format, ...);
extern double G_ReadDouble(FILE* f);
extern long G_ReadLong(FILE* f);
extern std::string G_ReadString(FILE* f);
extern std::string G_GetCurrentDir();

extern bool G_WriteDetectorDataToInternalXML(char * pWorkingFolder);
extern bool G_InternalDataInterface(char* pUrlXml,char* pWorkingFolder);
extern bool G_WriteDetectorDataToTextFile(char * pWorkingFolder);

extern int ReadRealTimeData();
extern int ReadRealTimeData_kc();//Add by Zihan Hong 20170327 for KC IMRCP project
extern void readDetctorInformation();
////////////////////////////////////////////////////////////////////////