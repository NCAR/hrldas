#=======================Multi-pass Land Data Assimilation Scheme (MLDAS)======point scale========================
#This program assimilated the soil moisture, leaf area index(LAI), and solar-induced chlorophyll fluorescence (SIF)
#Created by: Tongren Xu (xutr@bnu.edu.cn) and Xinlei He (hxlbsd@mail.bnu.edu.cn)

#module load python/3.7.5
#pip install --user numpy
#pip install --user pandas
#pip install --user netCDF4
#python3 ./MP-Crop-MLDAS-draft-v1.0.py &>nohup.out&

#Import dependent libraries
import numpy as np
import pandas as pd
import netCDF4 as nc
import os

#Set up data assimilation parameters
err_soilm_1=0.05                                       # error in modeling of soil moisture,                     Adjustable!
err_soilm_2=0.05                                       # error in modeling of soil moisture,                     Adjustable!
err_soilm_3=0.05                                       # error in modeling of soil moisture,                     Adjustable!
err_soilm_4=0.05                                       # error in modeling of soil moisture,                     Adjustable!
err_lai=0.1                                            # error in modeling of leaf area index,                   Adjustable!
err_lfmass=0.05                                        # error in modeling of leaf mass,                         Adjustable!
err_vcmax25=0.2                                        # error in modeling of vcmax25,                           Adjustable!
err_BIO2LAI=0.1                                        # error in modeling of SLA,                               Adjustable!

#Observation errors
err_soilm_o=0.005                                      # error in soil moisture observation, unit is m3m-3,      Adjustable!!!
err_lai_o=0.1                                          # error in leaf area index observation, unit is m2m-2,    Adjustable!!!
err_lfmass_o=0.02
err_sif_o=0.01                                         # error in sif observations,                              Adjustable!!!

en_numb=30                                             # numbser of ensemble numbers,                            Adjustable!!!

istep=153                                              # time step to run the model                              Adjustable!!!

#DA_LAI_1: optimize SLA with assimilation of LAI observations
#DA_LAI_2: update leaf biomass with assimilation of LAI observations
#DA_SM: update four-layer soil moisture with assimilation of surface soil moisture observations
#DA_SIF: optimize Vcmax with assimilation of SIF observations

DA_LAI_1=True                                          # TRUE or FALSE to determine whether assimilate leaf area index or not     Adjustable!!!
DA_LAI_2=True                                          # TRUE or FALSE to determine whether assimilate leaf area index or not     Adjustable!!!
DA_SM=True                                             # TRUE or FALSE to determine whether assimilate soil moisture or not     Adjustable!!!
DA_SIF=True                                            # TRUE or FALSE to determine whether assimilate SIF or not     Adjustable!!!

matrix_num=[0 for i in range(154)]
matrix_num=np.asmatrix(matrix_num)
BIO2LAI_result=matrix_num
vcmax25_result=matrix_num
en_sim_GPP=0*np.random.rand(en_numb)
en_sim_GPP=np.asmatrix(en_sim_GPP)
en_sim_GPP=np.transpose(en_sim_GPP)

#Set file path
path_restart="./restart/"
path_output="./output/"
path_output_final="./output_final/"
path_obs="./Bondville_obs_2001.dat"
path_namelist="./namelist.hrldas"
path_MPTABLE="./MPTABLE.TBL"
#Initial time
initial_time_step="2001050100"

#This function is used to create white noise, 'mean' is the mean, 'std' is the standard deviation
def add_noise(ori,n,mean,std):
    df=(ori+std*np.random.randn(n)+mean).reshape(n,1)
    return np.matrix(df)
            
#This function is used to write into a file
def writeLines(data,path):
    file=open(path,'a')
    file.seek(0)
    file.truncate()
    for i in range(len(data)):
        s = str(data[i]).replace('[','').replace(']','') 
        file.write(s)
    file.close()

#This function is used to query whether the content exists and return the subscript
def find_all_index(arr, item):
    i=0
    for line in arr:
        if item in line:
            return i
        i=i+1

#Restart file path
file_restart=path_restart+'RESTART.'+initial_time_step+'_DOMAIN1'

#Read and revise namelist.hrldas flie
namelist_hrldas=open(path_namelist)
namelist_data=namelist_hrldas.readlines()
id0=find_all_index(namelist_data,'RESTART_FILENAME_REQUESTED')
namelist_data_line=namelist_data[id0]
namelist_data_line=" RESTART_FILENAME_REQUESTED = "+ "\""+file_restart+"\""+"\n"
namelist_data[id0]=namelist_data_line
id1=find_all_index(namelist_data,'START_YEAR')
namelist_data[id1]=" START_YEAR  = "+ initial_time_step[0:4]+"\n"
id2=find_all_index(namelist_data,'START_MONTH')
namelist_data[id2]=" START_MONTH  = "+ initial_time_step[4:6]+"\n"
id3=find_all_index(namelist_data,'START_DAY')
namelist_data[id3]=" START_DAY  = "+ initial_time_step[6:8]+"\n"
writeLines(namelist_data,path_namelist)

#Read observations
observations=pd.read_table(path_obs,header=None,sep = " ")
time_step=observations[1]                                  
obs_lai=np.asmatrix(observations[2])                       # leaf area index observation
obs_soilm_1=np.asmatrix(observations[3])                   # surface soil moisture observation
obs_sif=np.asmatrix(observations[4])                       # solar-induced Fluorescence observation

# The outer time loop of MLDAS run
for ii in range(0,istep):
    #print(str(ii))
    if ii==0:
        # Open restart file and read state variables for the first time step, we should generate ensemble of restart file and MPtable
        file_restart=path_restart+"RESTART."+str(time_step[ii])+"_DOMAIN1"
        f=nc.Dataset(file_restart)
        sim_lai=np.asmatrix(f.variables['LAI'][:])
        sim_lfmass=np.asmatrix(f.variables['LFMASS'][:])
        sim_soilm=np.asmatrix(f.variables['SMC'][:])
        sim_soilt=np.asmatrix(f.variables['TG'][:])
        f.close()
        
        # Add noise to state variables in restart file and MPtable
        en_sim_lai=add_noise(sim_lai[0],en_numb,0,sim_lai[0]*err_lai)
        en_sim_lfmass=add_noise(sim_lfmass[0],en_numb,0,sim_lfmass[0]*err_lfmass)
        en_sim_soilm_1=add_noise(sim_soilm[0,0],en_numb,0,sim_soilm[0,0]*err_soilm_1)
        en_sim_soilm_2=add_noise(sim_soilm[0,1],en_numb,0,sim_soilm[0,1]*err_soilm_2)
        en_sim_soilm_3=add_noise(sim_soilm[0,2],en_numb,0,sim_soilm[0,2]*err_soilm_3)
        en_sim_soilm_4=add_noise(sim_soilm[0,3],en_numb,0,sim_soilm[0,3]*err_soilm_4)
        en_sim_soilm_layers=np.hstack((en_sim_soilm_1,en_sim_soilm_2,en_sim_soilm_3,en_sim_soilm_4))#Soil moisture has 4 layers
                
        # Generate en_numb of new restart files with the origional restart file, write state variables in new restart file
        for i in range(0,en_numb):
            file_restart_stand=path_restart+"RESTART."+str(time_step[ii])+"_DOMAIN1"
            file_restart=path_restart+"RESTART."+str(time_step[ii])+"_DOMAIN1_en"+str(i+1)
            path_rest="cp "+file_restart_stand+ " "+ file_restart
            os.system(path_rest)
            f=nc.Dataset(file_restart,mode='a')
            data_tem=np.asarray(en_sim_lai[i])
            f.variables["LAI"][:]=data_tem.reshape(1,1)
            data_tem=np.asarray(en_sim_lfmass[i])
            f.variables["LFMASS"][:]=data_tem.reshape(1,1)
            data_tem=np.asarray(en_sim_soilm_layers[i,:])
            f.variables["SMC"][:]=data_tem.reshape(1,1,4,1)
            f.variables["SH2O"][:]=data_tem.reshape(1,1,4,1)
            f.close()
    else:
        # From the second time step, go here
        # Open restart file and read state variables
        file_restart=path_restart+"RESTART."+str(time_step[ii])+"_DOMAIN1"
        f=nc.Dataset(file_restart)
        sim_lai=np.asmatrix(f.variables['LAI'][:])
        sim_lfmass=np.asmatrix(f.variables['LFMASS'][:])
        sim_soilm=np.asmatrix(f.variables['SMC'][:])
        sim_soilt=np.asmatrix(f.variables['TG'][:])
        f.close()

        # Add noise to state variables in restart file and MPtable
        en_sim_lai=add_noise(sim_lai[0],en_numb,0,sim_lai[0]*err_lai)
        en_sim_lfmass=add_noise(sim_lfmass[0],en_numb,0,sim_lfmass[0]*err_lfmass)
        en_sim_soilm_1=add_noise(sim_soilm[0,0],en_numb,0,sim_soilm[0,0]*err_soilm_1)#
        en_sim_soilm_2=add_noise(sim_soilm[0,1],en_numb,0,sim_soilm[0,1]*err_soilm_2)#
        en_sim_soilm_3=add_noise(sim_soilm[0,2],en_numb,0,sim_soilm[0,2]*err_soilm_3)#
        en_sim_soilm_4=add_noise(sim_soilm[0,3],en_numb,0,sim_soilm[0,3]*err_soilm_4)#
        en_sim_soilm_layers=np.hstack((en_sim_soilm_1,en_sim_soilm_2,en_sim_soilm_3,en_sim_soilm_4))#Soil moisture has 4 layers

        # Generate en_numb of new restart files with the origional restart file, write state variables in new restart file
        for i in range(0,en_numb):
            file_restart_stand=path_restart+"RESTART."+str(time_step[ii])+"_DOMAIN1"
            file_restart=path_restart+"RESTART."+str(time_step[ii])+"_DOMAIN1_en"+str(i+1)
            path_rest="cp "+file_restart_stand+ " "+ file_restart
            os.system(path_rest)
            f=nc.Dataset(file_restart,mode='a')
            data_tem=np.asarray(en_sim_lai[i])
            f.variables["LAI"][:]=data_tem.reshape(1,1)
            data_tem=np.asarray(en_sim_lfmass[i])
            f.variables["LFMASS"][:]=data_tem.reshape(1,1)
            data_tem=np.asarray(en_sim_soilm_layers[i,:])
            f.variables["SMC"][:]=data_tem.reshape(1,1,4,1)
            f.variables["SH2O"][:]=data_tem.reshape(1,1,4,1)
            f.close()

    # Run HRLDAS with new restart file and MPtable for en_numb times
    file_restart=path_restart+"RESTART."+str(time_step[ii])+"_DOMAIN1"
    namelist_hrldas=open(path_namelist)
    namelist_data=namelist_hrldas.readlines()
    namelist_data_line=namelist_data[id0]
    namelist_data_line=" RESTART_FILENAME_REQUESTED = "+ "\""+file_restart+"\""+"\n"
    namelist_data[id0]=namelist_data_line
    namelist_data[id1]=" START_YEAR  = "+ initial_time_step[0:4]+"\n"
    namelist_data[id2]=" START_MONTH  = "+ initial_time_step[4:6]+"\n"
    namelist_data[id3]=" START_DAY  = "+ initial_time_step[6:8]+"\n"
    writeLines(namelist_data,path_namelist)
    os.system("./hrldas.exe")
    file_restart_new_stand=path_output+"RESTART."+str(time_step[ii+1])+"_DOMAIN1"
    file_restart_new=path_restart+"RESTART."+str(time_step[ii+1])+"_DOMAIN1"
    path_rest="cp "+file_restart_new_stand+ " "+ file_restart_new
    os.system(path_rest)
    file_restart_out_1=path_output+str(time_step[ii])+"00.LDASOUT_DOMAIN1"
    file_restart_out_2=path_output_final+str(time_step[ii])+"00.LDASOUT_DOMAIN1"
    path_rest="cp "+file_restart_out_1+ " "+ file_restart_out_2
    os.system(path_rest)

    for i in range(0,en_numb):
        # Run HRLDAS with new restart file and MPtable for en_numb times
        file_restart=path_restart+"RESTART."+str(time_step[ii])+"_DOMAIN1_en"+str(i+1)
        namelist_hrldas=open(path_namelist)
        namelist_data=namelist_hrldas.readlines()
        namelist_data_line=namelist_data[id0]
        namelist_data_line=" RESTART_FILENAME_REQUESTED = "+ "\""+file_restart+"\""+"\n"
        writeLines(namelist_data,path_namelist)
        os.system("./hrldas.exe")
        file_restart_new_stand=path_output+"RESTART."+str(time_step[ii+1])+"_DOMAIN1"
        file_restart_new=path_output+"RESTART."+str(time_step[ii+1])+"_DOMAIN1_en"+str(i+1)
        path_rest="cp "+file_restart_new_stand+ " "+ file_restart_new
        os.system(path_rest)
        file_output_1=path_output+str(time_step[ii])+"00.LDASOUT_DOMAIN1"
        file_output_2=path_output+str(time_step[ii])+"00.LDASOUT_DOMAIN1_en"+str(i+1)
        path_rest="cp "+file_output_1+ " "+ file_output_2
        os.system(path_rest)
    # Read initial results from restart and output file
    # Read initial LAI, LFMASS, and soil moisture
    for i in range(0,en_numb):
        file_restart=path_output+"RESTART."+str(time_step[ii+1])+"_DOMAIN1_en"+str(i+1)
        f=nc.Dataset(file_restart)
        en_sim_lai[i,0]=np.asmatrix(f.variables['LAI'][:])
        en_sim_lfmass[i,0]=np.asmatrix(f.variables['LFMASS'][:])
        sim_soilm=np.asmatrix(f.variables['SMC'][:])
        en_sim_soilm_1[i,0]=sim_soilm[0,0]
        en_sim_soilm_2[i,0]=sim_soilm[0,1]
        en_sim_soilm_3[i,0]=sim_soilm[0,2]
        en_sim_soilm_4[i,0]=sim_soilm[0,3]
        f.close()
    # Read initial GPP
    for i in range(0,en_numb):
        file_ouput=path_output+str(time_step[ii])+"00.LDASOUT_DOMAIN1_en"+str(i+1)
        f=nc.Dataset(file_ouput)
        GPP_output=f.variables['GPP'][:]
        en_sim_GPP[i]=np.asmatrix(np.mean(GPP_output[1:25]))
        f.close()
    # Set up initial SLA
    MPTABLE_0=open(path_MPTABLE)
    MPTABLE_data=MPTABLE_0.readlines()
    id4=find_all_index(MPTABLE_data,'BIO2LAI    =')
    MPTABLE_data_line=MPTABLE_data[id4]
    
    if ii==0:
        BIO2LAI_num=round(0.015,3)
        MPTABLE_data_line="BIO2LAI    =  "+ str(BIO2LAI_num)+ ",  0.030,  0.015,  0.015,  0.015,  ! leaf are per living leaf biomass [m^2/kg]"+"\n"
        MPTABLE_data[id4]=MPTABLE_data_line
        writeLines(MPTABLE_data,path_MPTABLE)
        
    if DA_LAI_1==True:
        #Pass I: optimize SLA
        if obs_lai[0,ii+1]!=-999:
            #Read SLA from MPTABLE
            MPTABLE_0=open(path_MPTABLE)
            MPTABLE_data=MPTABLE_0.readlines()
            MPTABLE_data_line=MPTABLE_data[id4]
            sim_BIO2LAI =MPTABLE_data_line[14:19]
            BIO2LAI=float(sim_BIO2LAI)

            en_sim_BIO2LAI=BIO2LAI+BIO2LAI*err_BIO2LAI*np.random.randn(en_numb)
            en_sim_BIO2LAI=np.asmatrix(en_sim_BIO2LAI).T

            en_obs_lai=obs_lai[0,ii+1]+obs_lai[0,ii+1]*err_lai_o*np.random.randn(en_numb)
            en_obs_lai=np.asmatrix(en_obs_lai).T

            epsilon1=en_sim_lai-en_sim_lai.mean()
            diff1=np.square(epsilon1).sum()
            epsilon2=en_obs_lai-obs_lai[0,ii+1]
            diff2=np.square(epsilon2).sum()
            np.multiply(epsilon1,epsilon1)
            diff3=np.multiply((en_sim_BIO2LAI-(en_sim_BIO2LAI).mean()),(en_sim_lai-(en_sim_lai).mean())).sum()
            en_sim_BIO2LAI_model=en_sim_BIO2LAI+1/(diff1+diff2)*diff3*(en_obs_lai-en_sim_lai)

            BIO2LAI_num_1=(en_sim_BIO2LAI_model).mean()
            if BIO2LAI_num_1<=0.01:
                BIO2LAI_num_1=0.01
            
            if BIO2LAI_num_1>=0.04:
                BIO2LAI_num_1=0.04
            
            MPTABLE_data_line="BIO2LAI    =  "+ str(('%.3f'%BIO2LAI_num_1))+ ",  0.030,  0.015,  0.015,  0.015,  ! leaf are per living leaf biomass [m^2/kg]"+"\n"
            MPTABLE_data[id4]=MPTABLE_data_line
            writeLines(MPTABLE_data,path_MPTABLE)
            BIO2LAI_result[0,ii]=BIO2LAI_num_1
          
            
    if DA_LAI_2==True:
        #Pass II: update leaf biomass
        if obs_lai[0,ii+1]!=-999:
            MPTABLE_0=open(path_MPTABLE)
            MPTABLE_data=MPTABLE_0.readlines()
            MPTABLE_data_line=MPTABLE_data[id4]
            sim_BIO2LAI = MPTABLE_data_line[14:19]
            BIO2LAI=float(sim_BIO2LAI)
            obs_lai_lfmass=obs_lai[0,ii+1]/BIO2LAI
            en_obs_lai_lfmass=obs_lai_lfmass+obs_lai_lfmass*err_lfmass_o*np.random.randn(en_numb)
            en_obs_lai_lfmass=np.asmatrix(en_obs_lai_lfmass).T

            epsilon1=en_sim_lfmass-(en_sim_lfmass).mean()
            diff1=np.square(epsilon1).sum()
            epsilon2=en_obs_lai_lfmass-obs_lai_lfmass
            diff2=np.square(epsilon2).sum()
            diff3=np.multiply((en_sim_lfmass-(en_sim_lfmass).mean()),(en_sim_lfmass-(en_sim_lfmass).mean())).sum()
            en_sim_lfmass_model=en_sim_lfmass+1/(diff1+diff2)*diff3*(en_obs_lai_lfmass-en_sim_lfmass)

            file_restart=path_restart+"RESTART."+str(time_step[ii+1])+"_DOMAIN1"
            f=nc.Dataset(file_restart,mode='a')
            data_tem=np.asarray(en_sim_lfmass_model.mean())
            f.variables["LFMASS"][:]=data_tem.reshape(1,1)
            f.close()

    if DA_SM==True:
        #Pass III: update four-layer soil moisture
        if obs_soilm_1[0,ii+1]!=-999:
            en_obs_soilm_1=obs_soilm_1[0,ii+1]+obs_soilm_1[0,ii+1]*err_soilm_o*np.random.randn(en_numb)
            en_obs_soilm_1=np.asmatrix(en_obs_soilm_1).T

            epsilon1=en_sim_soilm_1-(en_sim_soilm_1).mean()
            diff1=np.square(epsilon1).sum()
            epsilon2=en_obs_soilm_1-obs_soilm_1[0,ii+1]
            diff2=np.square(epsilon2).sum()
            diff3=np.multiply((en_sim_soilm_1-(en_sim_soilm_1).mean()),(en_sim_soilm_1-(en_sim_soilm_1).mean())).sum()
            en_sim_soilm_model_1=en_sim_soilm_1+1/(diff1+diff2)*diff3*(en_obs_soilm_1-en_sim_soilm_1)

            epsilon1=en_sim_soilm_2-(en_sim_soilm_2).mean()
            diff1=np.square(epsilon1).sum()
            epsilon2=en_obs_soilm_1-obs_soilm_1[0,ii+1]
            diff2=np.square(epsilon2).sum()
            diff3=np.multiply((en_sim_soilm_2-(en_sim_soilm_2).mean()),(en_sim_soilm_2-(en_sim_soilm_2).mean())).sum()
            en_sim_soilm_model_2=en_sim_soilm_2+1/(diff1+diff2)*diff3*(en_obs_soilm_1-en_sim_soilm_2)

            epsilon1=en_sim_soilm_3-(en_sim_soilm_3).mean()
            diff1=np.square(epsilon1).sum()
            epsilon2=en_obs_soilm_1-obs_soilm_1[0,ii+1]
            diff2=np.square(epsilon2).sum()
            diff3=np.multiply((en_sim_soilm_3-(en_sim_soilm_3).mean()),(en_sim_soilm_3-(en_sim_soilm_3).mean())).sum()
            en_sim_soilm_model_3=en_sim_soilm_3+1/(diff1+diff2)*diff3*(en_obs_soilm_1-en_sim_soilm_3)

            epsilon1=en_sim_soilm_4-(en_sim_soilm_4).mean()
            diff1=np.square(epsilon1).sum()
            epsilon2=en_obs_soilm_1-obs_soilm_1[0,ii+1]
            diff2=np.square(epsilon2).sum()
            diff3=np.multiply((en_sim_soilm_4-(en_sim_soilm_4).mean()),(en_sim_soilm_4-(en_sim_soilm_4).mean())).sum()
            en_sim_soilm_model_4=en_sim_soilm_4+1/(diff1+diff2)*diff3*(en_obs_soilm_1-en_sim_soilm_4)

            file_restart=path_restart+"RESTART."+str(time_step[ii+1])+"_DOMAIN1"
            f=nc.Dataset(file_restart,mode='a')
            en_sim_soilm_layers_1=np.vstack(((en_sim_soilm_model_1).mean(),(en_sim_soilm_model_2).mean(),(en_sim_soilm_model_3).mean(),(en_sim_soilm_model_4).mean()))
            data_tem=np.asarray(en_sim_soilm_layers_1)
            f.variables["SMC"][:]=data_tem.reshape(1,1,4,1)
            f.variables["SH2O"][:]=data_tem.reshape(1,1,4,1)
            f.close()



    MPTABLE_0=open(path_MPTABLE)
    MPTABLE_data=MPTABLE_0.readlines()
    id5=find_all_index(MPTABLE_data,'VCMX25     =')
    MPTABLE_data_line=MPTABLE_data[id5]
    
    if ii==0:
        vcmax25_num=round(100.0,1)
        MPTABLE_data_line="VCMX25     =   "+ str(('%.1f'%vcmax25_num))+ ",   80.0,   60.0,   60.0,   55.0,  !"+"\n"
        MPTABLE_data[id5]=MPTABLE_data_line
        writeLines(MPTABLE_data,path_MPTABLE)
    
    if DA_SIF==True:
        #Pass IV: optimize Vcmax
        if obs_sif[0,ii]!=-999:
        
            MPTABLE_0=open(path_MPTABLE)
            MPTABLE_data=MPTABLE_0.readlines()
            MPTABLE_data_line=MPTABLE_data[id5]
            sim_vcmax25=MPTABLE_data_line[15:19]
            vcmax25=float(sim_vcmax25)
            en_sim_vcmax25=vcmax25+vcmax25*err_vcmax25*np.random.randn(en_numb)
            en_sim_vcmax25=np.asmatrix(en_sim_vcmax25).T

            #Observation operator: the relationship between SIF and GPP (SIF = a × GPP + b) 
            en_sim_sif=en_sim_GPP*8.047043323608+0.006940390680754
            en_obs_sif=obs_sif[0,ii]+obs_sif[0,ii]*err_sif_o*np.random.randn(en_numb)
            en_obs_sif=np.asmatrix(en_obs_sif).T

            epsilon1=en_sim_sif-(en_sim_sif).mean()
            diff1=np.square(epsilon1).sum()
            epsilon2=en_obs_sif-obs_sif[0,ii]
            diff2=np.square(epsilon2).sum()
            diff3=np.multiply((en_sim_vcmax25-(en_sim_vcmax25).mean()),(en_sim_sif-(en_sim_sif).mean())).sum()
            en_sim_vcmax25_model=en_sim_vcmax25+1/(diff1+diff2)*diff3*(en_obs_sif-en_sim_sif)

            vcmax25_num_1=(en_sim_vcmax25_model).mean()
            if vcmax25_num_1<=30:
                vcmax25_num_1=30
            
            if vcmax25_num_1>=200:
                vcmax25_num_1=200
            
            MPTABLE_data_line="VCMX25     =   "+str(('%.1f'%vcmax25_num_1))+ ",   80.0,   60.0,   60.0,   55.0,  !"+"\n"
            MPTABLE_data[id5]=MPTABLE_data_line
            writeLines(MPTABLE_data, path_MPTABLE)
            vcmax25_result[0,ii]=vcmax25_num_1
    #Remove ensemble flies
    path_rest1= "rm -f "+ path_restart+ "*en*"
    path_rest2="rm -f "+ path_output+ "*en*"
    os.system(path_rest1)
    os.system(path_rest2)

writeLines(BIO2LAI_result,"./BIO2LAI_result.dat")
writeLines(vcmax25_result,"./vcmax25_result.dat")