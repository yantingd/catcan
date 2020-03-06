#pragma rtGlobals=1		// Use modern global access method.
//this is for practice git
//this is the 2nd git practice, test
Function general_load(filepath, name_base,name_ending,num_of_file,ini,renamew)
         String filepath, name_base,name_ending,renamew
         Variable num_of_file,ini
         Variable/G k=ini
         Do 
             String wavetoload=filepath+name_base+num2str(k)+name_ending
             String newname=renamew+num2str(k)
             String wavelength="wavelegnth2"
             LoadWave/G/D/O wavetoload
             if (k>ini)
                KillWaves wave0
             else
                Rename wave0 wavelength
             endif
              Duplicate wave1 $newname
              KillWaves wave1
               k+=1
         While (k<num_of_file)
         
End



Function FreqWaves(sig,magE)
	Wave sig
	String magE 
	Variable wavescaling = (3.2*2)/(0.3*numpnts(sig)) //3.2 mm is the total discance scanned, if for north/south table 3.2mm for 3.4mm
	Wave Freq_THz, Freq_cm
	Duplicate $magE tempFreq_THz
	Variable/G k=0
	do
		tempFreq_THz[k]= k*1/(16384*wavescaling)  //4096 is the padding for the FFT
		k += 1
	while (k < numpnts(tempFreq_THz))
	Duplicate tempFreq_THz Freq_THz
	Duplicate $magE tempFreq_cm
	tempFreq_cm = Freq_THz/0.03
	Duplicate tempFreq_cm Freq_cm
	KillWaves tempFreq_THz, tempFreq_cm

End


Function LoadWaves4OCP(filepath,samplename,signame, sigrefname) //download the signal of both sample and reference at current temp
//file path: C:\\Users\\Yanting\\Desktop\\, 
//samplename is "ocp_" or "ocpr_", 
//temp is #, 
//signame is "sig_ocp", sigrefname is "sig_refocp" 
//ocp can be ocpo or ocpr
	String filepath, samplename
	String signame, sigrefname 
	Wave temperature 
	Variable/G k=0
	
	Do
                  Variable t=temperature[k]	
	           String wavetoload = filepath+samplename+num2str(t)+"K_tr"			
                  LoadWave/G/D/O wavetoload
                  String sig0 = signame+num2str(t)+"K_0"
                  String sig1 = signame+num2str(t)+"K_1"
                  String sig2 = signame+num2str(t)+"K_2"     
                  String ref0 = sigrefname+num2str(t)+"K_0"
                  String ref1 = sigrefname+num2str(t)+"K_1"
                  String ref2 = sigrefname+num2str(t)+"K_2"
                         
                 Duplicate wave1 $ref0
                 Duplicate wave6 $ref1
                 Duplicate wave11 $ref2       
                 Duplicate wave2 $sig0
                 Duplicate wave7 $sig1
                 Duplicate wave12 $sig2
       
                 Variable/G m=0
                 do
       	           String wavetokill = "wave" + num2str(m)
       	           Killwaves $wavetokill
       	           m+=1							
                 while (m-1<16)	
                 Display $ref0, $ref1, $ref2, $sig0, $sig1, $sig2 
                 Legend/C/N=text0/A=MC	
                 k+=1
        while(k<numpnts(temperature))		
	
End

Function MinSample(samplename,scale)
                 String samplename
                 Variable scale
                 //Variable sampleregion1,sampleregion2,refregion1,refregion2
                 Wave temperature
                 
                 Variable/G k=0
                 
                 Do 
                      Variable t=temperature[k] 
                      String sample=samplename+num2str(t)+"K"
                      String wave0=samplename+num2str(t)+"K_0"
                      String wave1=samplename+num2str(t)+"K_1"   
                      String wave2=samplename+num2str(t)+"K_2"
                      variable peakmin=min(min(WaveMax($wave0),WaveMax($wave1)),WaveMax($wave2))
                      if (peakmin==WaveMax($wave0))
              	       Duplicate $wave0 $sample						// Execute if condition 1 is TRUE
                      elseif (peakmin==WaveMax($wave1))
              	       Duplicate $wave1 $sample						// Execute if condition 2 is TRUE and condition 1 is FALSE
             
                      else
              	       Duplicate $wave2 $sample					// Optionally execute if all conditions are FALSE
                      endif         
                          SetScale/P x 0,scale,"", $sample
                k+=1
               While (k<numpnts(temperature))                          
                 
                 
                 
End

Function ChangeUnit(base,ending,scaling)

              String base, ending
              Variable scaling
              Wave temperature
              Variable/G k=0
              Do
                  Variable t=temperature[k]
                  String sample="fit_scl"+base+num2str(t)+ending
                  String scale_sample="fit_"+base+num2str(t)
                  //String scale_sample=base+num2str(t)+ending
                  Duplicate $sample temp
                  temp=temp/scaling
                  Duplicate temp $scale_sample
                  KillWaves temp
                  
               k+=1
               While(k<numpnts(temperature))
End


Function MaxRef(refname,scale)
                 String refname
                  Variable scale
                 //Variable sampleregion1,sampleregion2,refregion1,refregion2
                 Wave temperature
                
                 Variable/G k=0
                 Do
                      Variable t=temperature[k] 
                      String ref=refname+num2str(t)+"K"
                      String wave0=refname+num2str(t)+"K_0"
                      String wave1=refname+num2str(t)+"K_1"   
                      String wave2=refname+num2str(t)+"K_2"
                      variable peakmax=max(max(WaveMax($wave0),WaveMax($wave1)),WaveMax($wave2))
                      if (peakmax==WaveMax($wave0))
              	       Duplicate $wave0 $ref					// Execute if condition 1 is TRUE
                      elseif (peakmax==WaveMax($wave1))
              	       Duplicate $wave1 $ref						// Execute if condition 2 is TRUE and condition 1 is FALSE
             
                      else
              	       Duplicate $wave2 $ref				// Optionally execute if all conditions are FALSE
                      endif
                      SetScale/P x 0,scale,"", $ref
                      k+=1
                While (k<numpnts(temperature))  
End


Function AvgWaves4OCP(sig,samplename, numofwaves,scale) // doing average and wave scaling
//samplename is "ocp" or "refocp"

             String samplename,sig
             Variable numofwaves, scale
             Wave temperature
             Variable/G k=0
             
             Do
                    Variable t=temperature[k]            
                    String wave0=sig+samplename+num2str(t)+"K_0"
                    String wave1=sig+samplename+num2str(t)+"K_1"             
                    String avgsig="avg" +sig+samplename+num2str(t)+"K"
                    Duplicate $wave0 temp0
                    Duplicate $wave1 temp1             
                    Duplicate $wave1 tempavesig
                    if (numofwaves==2)               
                            tempavesig = (temp1+temp0)/2                    			
                    else
                           String wave2=sig+samplename+num2str(t)+"K_2"
                           Duplicate $wave2 temp2
             	 tempavesig = (temp1+temp2+temp0)/3					
                    endif
                    SetScale/P x 0,scale,"", tempavesig
                    Duplicate tempavesig $avgsig
                    KillWaves temp1, temp0, tempavesig
                    if (numofwaves==3)  
                           KillWaves temp2
                    endif
                    k+=1
             While (k<numpnts(temperature))             	

End


Function PLOT(samplename,refname)
             String samplename,refname
            
             Variable/G k=0
              Wave temperature
             
             Do
                    Variable t=temperature[k]   
                    String sample=samplename+num2str(t)+"K"
                    String ref=refname+num2str(t)+"K"
                    Display $sample,$ref
                    ModifyGraph rgb($ref)=(4352,4352,4352)
                    Legend/C/N=text0/A=MC
                    k+=1
             While (k<numpnts(temperature))     


End

Function FFT4ocp(sig,sigref, sample, sampleref,startpt,endpt) // calculate magE and phase m then magT for both sample and ref at the current temp
// sig is "avgsig_ocp", 
//sigref is "avgsig_refocp", 
//sample is "ocp", sampleref is "refocp", 
//ocp can be ocpo or ocpr
	String sig, sigref, sample, sampleref
	Variable startpt, endpt
	Wave temperature
	Variable/G k=0
	Do 
	       Variable t=temperature[k]		
		String name = sig+num2str(t)+"K"
		String nameref = sigref+num2str(t)+"K"
		String magesample = "magE_"+sample+num2str(t)+"K"
		String phssample = "phs_"+sample+num2str(t)+"K"
		String mageref = "magE_"+sampleref+num2str(t)+"K"
		String phsref = "phs_"+sampleref+num2str(t)+"K"
		String phit="phit_"+sample+num2str(t)+"K"
		String magt="magT_"+sample+num2str(t)+"K"
		
		Duplicate $name tempname
		Duplicate $nameref tempnameref
		
		FFT/OUT=3/PAD={16384}/WINF=HANNING/RP=[startpt,endpt]/DEST=magetempname tempname
		FFT/OUT=5/PAD={16384}/WINF=HANNING/RP=[startpt,endpt]/DEST=phstempname tempname
		FFT/OUT=3/PAD={16384}/WINF=HANNING/RP=[startpt,endpt]/DEST=magetempnameref tempnameref
		FFT/OUT=5/PAD={16384}/WINF=HANNING/RP=[startpt,endpt]/DEST=phstempnameref tempnameref
		
		Duplicate phstempname tempphit
		Unwrap 6.28319, phstempname, phstempnameref
		tempphit=phstempname-phstempnameref
		Duplicate magetempname tempmagt
		tempmagt=magetempname/magetempnameref
		Duplicate tempmagt $magt
		Duplicate magetempname $magesample
		Duplicate phstempname $phssample
		Duplicate magetempnameref $mageref
		Duplicate phstempnameref  $phsref
		Duplicate tempphit $phit
		Killwaves tempname, tempnameref, magetempname, phstempname, magetempnameref, phstempnameref, tempphit,tempmagt
	       k+=1
	While (k<numpnts(temperature))
	
End


Function FFT4ocpphase(sig,sigref, sample, sampleref,temp,startpt,endpt) // calculate magE and phase m then magT for both sample and ref at the current temp
// sig is "avgsig_ocp", 
//sigref is "avgsig_refocp", 
//sample is "ocp", sampleref is "refocp", 
//ocp can be ocpo or ocpr
	String sig, sigref, sample, sampleref
	Variable startpt, endpt, temp
		
		String name = sig+num2str(temp)+"K"
		String nameref = sigref+num2str(temp)+"K"
		String phssample = "testphs_"+sample+num2str(temp)+"K"
		String phsref = "testphs_"+sampleref+num2str(temp)+"K"
		String phit="testphit_"+sample+num2str(temp)+"K"
		
		Duplicate $name tempname
		Duplicate $nameref tempnameref
		FFT/OUT=5/PAD={16384}/WINF=HANNING/RP=[startpt,endpt]/DEST=phstempname tempname
		FFT/OUT=5/PAD={16384}/WINF=HANNING/RP=[startpt,endpt]/DEST=phstempnameref tempnameref
		
		Duplicate phstempname tempphit
		Unwrap 6.28319, phstempname, phstempnameref
		tempphit=phstempname-phstempnameref
		Duplicate phstempname $phssample
		Duplicate phstempnameref  $phsref
		Duplicate tempphit $phit
		Killwaves tempname, tempnameref, phstempname, phstempnameref, tempphit
	
End



Function ScaleMagT(sample, scale) // scale is 1.18
              String sample
              Variable scale
              Wave temperature
              Variable/G k=0
              
              Do 
                     Variable t=temperature[k]
                     String rawmagT="magT_"+sample+num2str(t)+"K"
                     String scalemagT="scaledmagT_"+sample+num2str(t)+"K"
                     Duplicate $rawmagT temprawmagt, tempscalemagT
                     tempscalemagT=temprawmagT/scale
                     Duplicate tempscalemagT $scalemagT
                     KillWaves tempscalemagT, temprawmagT
                     k+=1
               While (k<numpnts(temperature))
End


Function LoadMapleOutput (filepath,sample,scalesample,scalefreq, thickness)  //file path: C:\\Users\\Yanting\\Desktop\\,
              String filepath,sample,scalesample,scalefreq             
              Variable thickness // in um
               Wave temperature
              Variable/G k=0
              Do
                      Variable t=temperature[k]
                      String wavetoload = filepath+"mapleOutput_"+sample+num2str(t)+"K.txt"
                      LoadWave/J/D/O wavetoload //need to fix???????"			
                  //LoadWave/J/D/O wavetoload
                      String freq_thickness=scalefreq+num2str(t)+"K_"+num2str(thickness)+"um"
                     // String index=sample+num2str(t)+"Kindex"
                      String index_thickness=scalesample+num2str(t)+"Kindex_"+num2str(thickness)+"um"
                      //String kappa=sample+num2str(t)+"Kkappa"
                      String kappa_thickness=scalesample+num2str(t)+"Kkappa_"+num2str(thickness)+"um"
                      //String alpha=sample+num2str(t)+"Kalpha"
                      String alpha_thickness=scalesample+num2str(t)+"Kalpha_"+num2str(thickness)+"um"
                      Duplicate wave0 $freq_thickness
                      Duplicate wave1 $index_thickness
                      Duplicate wave2 $kappa_thickness
                      Duplicate wave3 $alpha_thickness
                      KillWaves wave0, wave1, wave2, wave3
                      k+=1
               While (k<numpnts(temperature))
End


              
Function alphaVStemp(sample,numofpoint,freq,thickness)

              String sample            
              Variable numofpoint,freq,thickness
              String alpha=sample+"_alpha"+num2str(thickness)+"um_"+num2str(freq)+"THz" 
              Wave temperature     
                             
              
              Make/N=(numpnts(temperature))/D 'tempalpha'
              Variable/G k=0
              
              do 
                    Variable t=temperature[k]
                    String name=sample+num2str(t)+"Kalpha_"+num2str(thickness)+"um"
                    Duplicate $name tempname
                    tempalpha[k]=tempname[numofpoint]
                    KillWaves tempname
                    k+=1
              while (k<numpnts(temperature))
              
              Duplicate tempalpha $alpha
              KillWaves tempalpha
End



Function SaveMagtPhitFreq(sample,magT,phit)
              String sample,magT,phit
              Wave temperature,Freq_THz
              Variable/G k=0
              Do
                        Variable t=temperature[k]
                        String magttosave=magT+"_"+sample+num2str(t)+"K"
                        String phittosave=phit+"_"+sample+num2str(t)+"K"
                        String name=sample+num2str(t)+"K.txt"         
                        Save/G/W $magttosave,$phittosave, Freq_THz as name
                        k+=1
               While (k<numpnts(temperature))
End  


//Function CalMolarAbs(thickness, startfreq,deltafreq, endfreq,sample)    
//
  //            String sample
    //          Variable thichness
      //        Variable startfreq,endfreq,deltafreq
        //      String molarabs
          //    Variable/G k=startfreq
            //  Do
              //      String molarabs="molar_"+sample+"_alpha_"+num2str(K)+"THz"
                //    String abss=sample+"_alpha"+num2str(thickness)+"um_"+
                  //  Duplicate 
//End
                   
                   
                   
Function RenameWaves(base,WrongName, CorrectName,ending)                   
                   String base, WrongName, CorrectName,ending
                   Wave temperature
                   Variable/G k=0
                   Do
                            Variable t=temperature[k]
                            String wrongwavename=base+WrongName+num2str(t)+"K"+ending
                            String correctwavename=base+CorrectName+num2str(t)+"K"+ending
                            Rename $wrongwavename, $correctwavename
                            k+=1
                     While (k<numpnts(temperature))
End               


Function OffsetToAlpha(sample,thickness,DeltaOffset,inioffset)
                    String sample
                    Variable thickness, DeltaOffset
                    Variable inioffset
                    Wave temperature
                    Variable/G k=0
                    Do
                             Variable t=temperature[k]
                             Variable tempoffset=inioffset+DeltaOffset*k
                             String alphawave=sample+num2str(t)+"Kalpha_"+num2str(thickness)+"um"
                             ModifyGraph offset($alphawave)={0,tempoffset}     
                             k+=1
                    While  (k<numpnts(temperature))
End                       
                   
