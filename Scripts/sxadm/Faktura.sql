CREATE OR REPLACE FUNCTION FaktureraOrder(
    in_anvandare character varying,
    in_orderlista integer[])
  RETURNS integer AS
$BODY$
declare
	this_kundnr varchar;
	cn_kundnr integer;
	cn_lagernr integer;
	cn_saljare integer;
	cn_bonus integer;
	cn_ktid integer;
	cn_moms integer;
	cn_ordercnt integer;
	cn_lasta integer;
	cn_faktor integer;
	kund_ejfakturerbar integer;
	kund_bonus integer;
	kund_ranta integer;
	kund_sarfaktura integer;
	this_moms real; 
	this_momsproc real; 
	this_faktnr integer;
	this_lagernr integer;
	temp_ordernr integer;
	this_firstrun boolean;
	this_tattbetala real;
begin
	
	IF array_length(in_orderlista,1)<1 THEN raise EXCEPTION 'Inga ordrar begärda. Antal %', array_length(in_orderlista,1); END IF; 
	
	if not exists (select 1 from saljare where forkortning=in_anvandare) then raise exception 'Användare % saknas', in_anvandare; end if;
	
	SELECT 		max(kundnr), count(DISTINCT kundnr), count(DISTINCT lagernr), count(distinct trim(left(saljare,30))), count(distinct bonus ), 
				count(distinct ktid), count(distinct moms), COUNT(*), sum(case when wmslock is not null or lastdatum is not null then 1 else 0 end), count(distinct order1.faktor),
				max(moms), max(lagernr)
		INTO 	this_kundnr, cn_kundnr, cn_lagernr, cn_saljare, cn_bonus,
				cn_ktid, cn_moms  , cn_ordercnt, cn_lasta , cn_faktor,
				this_moms, this_lagernr
		FROM order1 WHERE ordernR = ANY(in_orderlista);
	
	
	IF cn_kundnr > 1 THEN raise EXCEPTION 'Flera kundnummer (%) är blandat i orderlistan.', cn_lagernr; END IF; 
	IF cn_lagernr > 1 THEN raise EXCEPTION 'Flera lagernummer (%) är blandat i orderlistan.', cn_lagernr; END IF; 
	IF cn_saljare > 1 THEN raise EXCEPTION 'Flera säljare (%) är blandat i orderlistan.', cn_saljare; END IF; 
	IF cn_bonus > 1 THEN raise EXCEPTION 'Flera bonusar (%) är blandat i orderlistan.', cn_bonus; END IF; 
	IF cn_ktid > 1 THEN raise EXCEPTION 'Flera kredittider (%) är blandat i orderlistan.', cn_ktid; END IF; 
	IF cn_moms > 1 THEN raise EXCEPTION 'Flera momssatser (%) är blandat i orderlistan.', cn_moms; END IF; 
	IF cn_ordercnt <> array_length(in_orderlista,1) THEN raise EXCEPTION 'Det är saknas ordrar. Antal begärda:% antal hittade:%', array_length(in_orderlista,1), cn_ordercnt; END IF; 
	IF cn_lasta > 0 THEN raise EXCEPTION '% order(s) är låsta. Antingen i WWMS eller för bearbetning.', cn_lasta; END IF; 
	IF cn_faktor > 1 THEN raise EXCEPTION 'Flera faktoringtyper (%) är blandat i orderlistan.', cn_faktor; END IF; 

	if not exists (select 1 from kund where nummer=this_kundnr) then raise exception 'Kundnummer % saknas', this_kundnr; end if;


	select ejfakturerbar, bonus, rantfakt , sarfaktura into kund_ejfakturerbar, kund_bonus, kund_ranta, kund_sarfaktura from kund where nummer=this_kundnr;
	if (kund_ejfakturerbar <> 0 ) THEN raise EXCEPTION 'Kund % är inte fakturerbar', this_kundnr; END IF;
	if (kund_bonus <> 0 ) THEN raise EXCEPTION 'Kund % har bonus som inte stöds just nu', this_kundnr; END IF;
	if (kund_ranta <> 0 ) THEN raise EXCEPTION 'Kund % har ranta som inte stöds just nu', this_kundnr; END IF;
	if (kund_sarfaktura <> 0 and array_length(in_orderlista,1)>1) THEN raise EXCEPTION 'Kund % har särfaktura. Anropa en gång per ordernr', this_kundnr; END IF;

	select max(faktnr)+1 into this_faktnr from faktura1;
	select case when this_moms = 1 then fuppg.moms1 else case when this_moms=2 then fuppg.moms2 else case when this_moms=3 then fuppg.moms3 else 0 end  end end into this_momsproc from fuppg;


	--Orderrader till temp
	create temporary table temprader (rad serial, artnr varchar, namn varchar, rab real, lev real, text varchar, pris real, summa real, 
									konto varchar, netto real, enh varchar, bon_nr integer, ordernr integer, rantafakturanr integer, 
									rantafalldatum date, rantabetaldatum date, rantabetalbelopp real, rantaproc real, stjid integer, momsfri boolean default false, levnr varchar)  on commit drop ;
								
	this_firstrun=true;
	foreach temp_ordernr in array in_orderlista loop
		if not this_firstrun then insert into temprader (ordernr, text) values (temp_ordernr, ''); end if; -- tomrad mellan ordrar
		this_firstrun = false;
		insert into temprader (ordernr, text) select ordernr, 'Följesedel: ' || ordernr || ' Datum: ' || datum from order1 where ordernr=temp_ordernr;	
		insert into temprader (ordernr, text) select ordernr, 'Märke: ' || coalesce(marke,'') from order1 where ordernr=temp_ordernr;	
		insert into temprader (ordernr, artnr, namn, rab, lev, text, pris, summa, konto, netto, enh, stjid, levnr) select
			ordernr, artnr, namn, rab, lev, text, pris, summa, konto, netto, enh, stjid, levnr
			from order2 where ordernr=temp_ordernr order by ordernr, pos;
	end loop;


--add ränta






-- add bonus






	--Fakturarader
	insert into faktura2 (faktnr, pos, prisnr, artnr, namn, rab, lev, text, 
				pris, summa, konto, netto, enh, bon_nr, ordernr, rantafakturanr, 
				rantafalldatum, rantabetaldatum, rantabetalbelopp , rantaproc, stjid)
			select this_faktnr, rad, 0, coalesce(artnr,''), coalesce(namn,''), coalesce(rab,0), coalesce(lev,0), coalesce(text,''),
				round(coalesce(pris,0)::numeric,2), round(coalesce(summa,0)::numeric,2), coalesce(konto,''), case when coalesce(netto,0)<> 0 then coalesce(netto,0) else coalesce(pris,0)*(1-coalesce(rab,0)/100) end, coalesce(enh,''), coalesce(bon_nr,0), ordernr, coalesce(rantafakturanr,0),
				rantafalldatum, rantabetaldatum, coalesce(rantabetalbelopp,0), coalesce(rantaproc,0), coalesce(stjid,0) from temprader order by rad;
	
	--Fakturan			
	insert into faktura1 (faktnr, datum, kundnr, namn, adr1, adr2, adr3, levadr1, 
						levadr2, levadr3, saljare, referens, marke, moms, ktid, ranta, 
						bonus, mottagarfrakt, levvillkor, fraktkundnr, fraktbolag, 
						fraktfrigrans, levdat, orderdat, ordernr, faktor, 
						text1, text2, text3, text4, text5, 
						faktortext1,  faktortext2,  faktortext3, 
						rantfakt , t_netto, t_moms, t_orut, t_attbetala, t_innetto, 
						lagernr, direktlevnr, momsproc, inkassostatus, inkassodatum)
					select	this_faktnr, current_date, this_kundnr, order1.namn , order1.adr1 , order1.adr2 , order1.adr3 , order1.levadr1 ,
						order1.levadr2 , order1.levadr3, order1.saljare , order1.referens , case when cn_ordercnt > 1 then '' else order1.marke end, order1.moms , order1.ktid , fuppg.droj ,
						order1.bonus , order1.mottagarfrakt , order1.levvillkor , order1.fraktkundnr , order1.fraktbolag ,
						order1.fraktfrigrans , case when cn_ordercnt > 1 then null else order1.levdat end , case when cn_ordercnt > 1 then null else order1.datum end, case when cn_ordercnt > 1 then 0 else order1.ordernr end, order1.faktor,  
						case when fuppg.temp_text >0 then fuppg.temp_text1 else case when order1.bonus >0 then fuppg.bon_text1 else fuppg.eb_text1 end end,
						case when fuppg.temp_text >0 then fuppg.temp_text2 else case when order1.bonus >0 then fuppg.bon_text2 else fuppg.eb_text2 end end,
						case when fuppg.temp_text >0 then fuppg.temp_text3 else case when order1.bonus >0 then fuppg.bon_text3 else fuppg.eb_text3 end end,
						case when fuppg.temp_text >0 then fuppg.temp_text4 else case when order1.bonus >0 then fuppg.bon_text4 else fuppg.eb_text4 end end,
						case when fuppg.temp_text >0 then fuppg.temp_text5 else case when order1.bonus >0 then fuppg.bon_text5 else fuppg.eb_text5 end end,
						case when order1.faktor > 0 then fuppg.faktortext1 else '' end,case when order1.faktor > 0 then fuppg.faktortext2 else '' end,case when order1.faktor > 0 then fuppg.faktortext3 else '' end, 
						kund.rantfakt, round(trader.summa::numeric,2), 
							round((trader.momspliktigt*this_momsproc/100)::numeric,2),
							0, 0, trader.netto,
							order1.lagernr , order1.direktlevnr , this_momsproc, null, null
					from kund, fuppg, order1 , (select sum(summa) as summa, sum(case when momsfri then 0 else summa end) as momspliktigt, sum(netto) as netto from temprader) trader
					where order1.ordernr = in_orderlista[1] and kund.nummer=order1.kundnr;
				

	update faktura1 set t_orut =  round((t_netto+t_moms)::numeric,0)-(t_netto+t_moms) where faktnr=this_faktnr;
	update faktura1 set t_attbetala =  round((t_netto+t_moms+t_orut)::numeric,0) where faktnr=this_faktnr;
	select t_attbetala into this_tattbetala from faktura1 where faktnr=this_faktnr;  			


	-- lagersaldo
	--undvik on conflict do update då den inte funkar på tabelelr med regler
	insert into lager as l (lagernr, artnr, ilager, bestpunkt, maxlager, iorder, best, hindrafilialbest)  
			select this_lagernr, artnr, 0, 0,0,0,0,0 from temprader where artnr in (select nummer from artikel) and not exists (select 1 from lager ll where ll.lagernr=this_lagernr and ll.artnr=temprader.artnr) group by artnr;
	update lager set ilager=ilager-q.antal, iorder=iorder-q.antal
		from (select artnr, sum(lev) as antal from temprader where artnr in (select nummer from artikel) group by artnr) as q
		where lager.lagernr=this_lagernr and lager.artnr=q.artnr;

	insert into lagerhand  (artnr, lagernr, datum, tid, anvandare, handelse, ordernr, stjid, gammaltilager, nyttilager, forandring, bestnr) 
		select artnr, this_lagernr, current_date, current_time, in_anvandare, 'Fakturerad', 0, 0, 0, 0, -sum(lev), 0 from temprader where artnr in (select nummer from artikel) group by artnr ;

	-- uppdatera stärnrader
	update stjarnrad set fakturanr = this_faktnr where stjid in (select stjid from temprader where stjid>0 and artnr like '*%');
	

	-- Kundreskontra
	if this_tattbetala <> 0 then
		insert into kundres (rantfakt, faktnr, kundnr, namn, tot, netto, datum, falldat, faktor, bonus, medelmomsproc)
			select rantfakt, faktnr, kundnr, namn, t_attbetala, t_netto, datum, datum+ktid, faktor, bonus, t_moms::numeric / t_netto::numeric * 100 from faktura1 where faktnr=this_faktnr;

		if (select faktor from faktura1 where faktnr=this_faktnr) <> 0 then 
			insert into faktorut (faktnr, datum, tot, kundnr, namn, falldat)
				select faktnr, datum, tot, kundnr, namn, falldat from kundres where faktnr=this_faktnr;
		end if;
	else -- om det är nullfaktura så lägg direkt i betald
		insert into betjour (rantfakt, faktnr, kundnr, namn, bet, betdat, betsatt, bonsumma, tallopnr, ar, man, pantsatt, betsattkonto)
			select rantfakt, faktnr, kundnr, namn, 0, datum, 'K', 0, 0, year(datum), month(datum), 0, 0 from faktura1 where faktnr=this_faktnr;
	end if;

	
	-- Artikelstatistik
	insert into artstat as a (artnr, ar, man, salda, tbidrag) 
		select artnr, year(current_date), month(current_date), 0, 0 from faktura2 f2
			where faktnr=this_faktnr and artnr in (select nummer from artikel)  
				and not exists (select 1 from artstat where artnr=f2.artnr and ar=year(current_date) and man = month(current_date))
			group by f2.artnr;
	update artstat set salda = salda+q.antal, tbidrag=tbidrag + q.tb
		from (select artnr, year(current_date) as ar, month(current_date) as man, sum(lev) as antal, sum(summa-netto*lev) as tb from faktura2 
				where faktnr=this_faktnr and artnr in (select nummer from artikel) group by artnr) q
		where artstat.artnr=q.artnr and artstat.ar=q.ar	and artstat.man = q.man;
				
	
	-- leverantörsstatistik
	insert into levstat (levnr, ar, man, ftot, ftbidrag, tot)
		select a.lev as levnr, year(current_date), month(current_date), 0, 0, 0   
			from faktura2 f2 join artikel a on a.nummer=f2.artnr join lev l on l.nummer=a.lev
			where f2.faktnr=this_faktnr and not exists (select from levstat where levnr=l.nummer and ar=year(current_date) and man=month(current_date))
			group by a.lev;
	update levstat set ftot=ftot+q.summa, ftbidrag=ftbidrag+q.tb
		from (select a.lev as levnr, sum(f2.summa) as summa, sum(f2.summa-f2.lev*f2.netto) as tb
			from faktura2 f2 join artikel a on a.nummer=f2.artnr join lev l on l.nummer=a.lev where trim(coalesce(a.lev,'')) <> ''  and f2.faktnr=this_faktnr 
			group by a.lev
		) q
	where levstat.levnr = q.levnr and levstat.ar=year(current_date) and levstat.man=month(current_date);
 	
				
	-- säljarestatistik
	insert into sljstat (saljare, ar, man, totalt, tbidrag) 
		select trim(substring(saljare, 1, 30)), year(current_date), month(current_date), t_netto, t_netto-t_innetto 
			from faktura1 
			where faktnr=this_faktnr and trim(substring(saljare, 1, 30)) in (select namn from saljare)
					and not exists (select from sljstat where saljare=trim(substring(saljare, 1, 30)) and ar=year(current_date) and man=month(current_date));
	update sljstat set totalt=sljstat.totalt + q.totalt, tbidrag = sljstat.tbidrag+q.tbidrag
		from (select trim(substring(saljare, 1, 30)) as saljare, t_netto as totalt, t_netto-t_innetto as tbidrag from faktura1 where faktnr=this_faktnr and trim(substring(saljare, 1, 30)) in (select namn from saljare)) q
	where sljstat.saljare= q.saljare and sljstat.ar=year(current_date) and sljstat.man=month(current_date);

	
	-- statistik
	insert into statistik as ss 	(ar, man, fak_netto, fak_moms, fak_attbetala, fak_innetto, fak_antal, 
									fak_rantatot, fak_rantaantal, kun_betalt, kun_betaltant, 
									kun_brhog, kun_brhogant, kun_brlag, kun_brlagant, 
									kun_bejreg, kun_bejregant, kun_rankost, lev_attbet, lev_attbetant, 
									lev_ranta, lev_rantaantal, lev_varu, lev_varuantal, lev_betalt, lev_betaltant,
									kun_rankostant)
			select year(current_date), month(current_date), 0, 0, 0, 0, 0,
					0, 0, 0, 0,
					0, 0, 0, 0,
					0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0 ,
					0
				where not exists (select from statistik where ar=year(current_date) and man = month(current_date));
	update statistik set fak_netto=statistik.fak_netto + q.t_netto,
							fak_moms=statistik.fak_moms+q.t_moms,
							fak_attbetala  = statistik.fak_attbetala + q.t_attbetala,
							fak_innetto = statistik.fak_innetto + q.t_innetto,
							fak_antal = statistik.fak_antal+1
		from (select t_netto, t_moms, t_attbetala, t_innetto from faktura1 where faktnr=this_faktnr) q
	where ar=year(current_date) and man = month(current_date);
	
-- set utlev1
	insert into utlev1 	(ordernr, dellev, namn, adr1, adr2, adr3, levadr1, levadr2, levadr3,
						saljare, referens, kundnr, marke, datum,
						moms, status, ktid, bonus, faktor, levdat, levvillkor,
						mottagarfrakt, fraktkundnr, fraktbolag, fraktfrigrans, 
						lagernr, returorder, direktlevnr, tid, faktnr, 
						veckolevdag, annanlevadress, ordermeddelande, wordernr, 
						linjenr1 , linjenr2 , linjenr3 ,
						kundordernr, forskatt, forskattbetald , betalsatt)
			select ordernr, dellev, namn, adr1, adr2, adr3, levadr1, levadr2, levadr3,
						saljare, referens, kundnr, marke, datum,
						moms, status, ktid, bonus, faktor, levdat, levvillkor,
						mottagarfrakt, fraktkundnr, fraktbolag, fraktfrigrans, 
						lagernr, returorder, direktlevnr, tid, this_faktnr, 
						veckolevdag, annanlevadress, ordermeddelande, wordernr, 
						linjenr1 , linjenr2 , linjenr3 ,
						kundordernr, forskatt, forskattbetald , betalsatt
				from order1 where ordernr = any(in_orderlista);
			
	
	
	

		
-- set orderhändelse fakturerad
	insert into orderhand (ordernr, datum, tid, anvandare, handelse, nyordernr, antalkolli, totalvikt) 
		select ordernr, current_date, current_time, in_anvandare, 'Fakturerad', 0, 0, 0 from unnest(in_orderlista) as ordernr;

-- delette ordewr
	delete from order1 where ordernr = any(in_orderlista);
	delete from order2 where ordernr = any(in_orderlista);


	return this_faktnr;
end;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

select FAKTURERAORDER('UB', ARRAY [438750, 438751]);