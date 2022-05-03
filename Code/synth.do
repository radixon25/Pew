ssc install synth 
ssc install mat2txt
#delimit; 
synth   fr  
            fr(2000) fr(2001) fr(2002) Ag(1997) Ag(1998)
			Pro(1997) Pro(1998) Prof(1997) Prof(1998) Man(1997)
			Man(1998) Cl(1997) Cl(1998) Sale(1997) Sale(1998) Serv(1997)
			Serv(1998)
            ,       
        trunit(36) trperiod(2003) unitnames(State) 
        mspeperiod(1985(1)2003) resultsperiod(1985(1)2010)
        keep(../data/synth/synth_bmprate.dta) replace fig;
        mat list e(V_matrix);
        #delimit cr
        graph save Graph ../Figures/synth_tx.gph, replace}