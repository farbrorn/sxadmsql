

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
	temp_ordernr integer;
	this_firstrun boolean;
	this_tattbetala real;
begin
	
	IF array_length(in_orderlista,1)<1 THEN raise EXCEPTION 'Inga ordrar begärda. Antal %', array_length(in_orderlista,1); END IF; 
	
	if not exists (select 1 from saljare where forkortning=in_anvandare) then raise exception 'Användare % saknas', in_anvandare; end if;
	
	SELECT 		max(kundnr), count(DISTINCT kundnr), count(DISTINCT lagernr), distinct(trim(left(saljare,30))), count(distinct bonus ), 
				count(distinct ktid), count(distinct moms), COUNT(*), sum(case when wmslock is not null or lastdatum is not null then 1 else o end), count(distinct order1.faktor),
				max(moms)
		INTO 	thins_kundnr, cn_kundnr, cn_lagernr, cn_saljare, cn_bonus
				cn_ktid, cn_moms  , cn_ordercnt, cn_lasta , cn_faktor,
				this_moms
		FROM order1 WHERE ordernr IN (in_orderlista);
	IF cn_kundnr > 1 THEN raise EXCEPTION 'Flera kundnummer (%) är blandat i orderlistan.', cn_lagernr; END IF; 
	IF cn_lagernr > 1 THEN raise EXCEPTION 'Flera lagernummer (%) är blandat i orderlistan.', cn_lagernr; END IF; 
	IF cn_saljare > 1 THEN raise EXCEPTION 'Flera säljare (%) är blandat i orderlistan.', cn_saljare; END IF; 
	IF cn_bonus > 1 THEN raise EXCEPTION 'Flera bonusar (%) är blandat i orderlistan.', cn_bonus; END IF; 
	IF cn_ktid > 1 THEN raise EXCEPTION 'Flera kredittider (%) är blandat i orderlistan.', cn_ktid; END IF; 
	IF cn_moms > 1 THEN raise EXCEPTION 'Flera momssatser (%) är blandat i orderlistan.', cn_moms; END IF; 
	IF cn_ordercnt <> array_length(in_orderlista,1) THEN raise EXCEPTION 'Det är saknas ordrar. Antal begärda:% antal hittade:%', array_length(in_orderlista,1), cn_ordercnt; END IF; 
	IF cn_lasta > 1 THEN raise EXCEPTION '% order(s) är låsta. Antingen i WWMS eller för bearbetning.', cn_lasta; END IF; 
	IF cn_faktor > 1 THEN raise EXCEPTION 'Flera faktoringtyper (%) är blandat i orderlistan.', cn_faktor; END IF; 

	if not exists (select 1 from kund where nummer=this_kundnr) then raise exception 'Kundnummer % saknas', this_kundnr; end if;

	select ejfakturerbar, bonus, ranta , sarfaktura into kund_ejfakturerbar, kund_bonus, kund_ranta, kund_sarfaktura from kund where kundnr=this_kundnr;
	if (kund_ejfakturerbar <> 0 ) THEN raise EXCEPTION 'Kund % är inte fakturerbar', this_kundnr; END IF;
	if (kund_bonus <> 0 ) THEN raise EXCEPTION 'Kund % har bonus som inte stöds just nu', this_kundnr; END IF;
	if (kund_ranta <> 0 ) THEN raise EXCEPTION 'Kund % har ranta som inte stöds just nu', this_kundnr; END IF;
	if (kund_sarfaktura <> 0 and array_length(in_orderlista,1)>1) THEN raise EXCEPTION 'Kund % har särfaktura. Anropa en gång per ordernr', this_kundnr; END IF;

	select max(faktnr)+1 into this_faktnr from faktura1;
	select case when this_moms = 1 then fuppg.moms1 else case when this_moms=2 then fuppg.moms2 else case when this_moms=3 then fuppg.moms3 else 0 end  end end into this_momsproc from fuppg;


	--Orderrader till temp
	create temporary table temprader (rad serial, artnr varchar, namn varchar, rab real, lev real, text varchar, pris real, summa real, 
									konto varchar, netto real, enh varchar, bon_nr integer, ordernr integer, rantafakturanr integer, 
									rantafalldatum date, rantabetaldatum date, rantebelopp real, rantaproc real, stjid integer, momsfri boolean default false, levnr varchar)  on commit drop ;
								
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
				rantafallfdatum, rantabetaldatum, rantabelopp, rantaproc, stjid)
			select this_faktnr, rad, 0, coalesce(artnr,''), coalesce(namn,''), coalesce(rab,0), coalesce(lev,0), coalesce(text,''),
				round(coalesce(pris,0)::numeric,2), round(coalesce(summa,0)::numeric,2), coalesce(konto,''), case when coalesce(netto,0)<> 0 then coalesce(netto,0) else coalesce(pris,0)*(1-coalesce(rab,0)/100) end, coalesce(enh,''), bon_nr, ordernr, rantafakturanr,
				rantafalldatum, rantabetaldatum, rantabelopp, rantaproc, coalesce(stjid,0)) from temprader order by rad;
	
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
						order1.fraktfrigrans , case when cn_ordercnt > 1 then null else order1.levdat end , case when cn_ordercnt > 1 then null else order1.datum end, case when cn_ordercnt > 1 then null else order1.ordernr end, order1.faktor  
						case when fuppg.temp_text >0 then fuppg.temp_text1 else case when order1.bonus >0 then fuppg.bon_text1 else fuppg.eb_text1 end,
						case when fuppg.temp_text >0 then fuppg.temp_text2 else case when order1.bonus >0 then fuppg.bon_text2 else fuppg.eb_text2 end,
						case when fuppg.temp_text >0 then fuppg.temp_text3 else case when order1.bonus >0 then fuppg.bon_text3 else fuppg.eb_text3 end,
						case when fuppg.temp_text >0 then fuppg.temp_text4 else case when order1.bonus >0 then fuppg.bon_text4 else fuppg.eb_text4 end,
						case when fuppg.temp_text >0 then fuppg.temp_text5 else case when order1.bonus >0 then fuppg.bon_text5 else fuppg.eb_text5 end,
						case when order1.faktor > 0 then fuppg.faktortext1 else '' end,case when order1.faktor > 0 then fuppg.faktortext2 else '' end,case when order1.faktor > 0 then fuppg.faktortext3 else '' end, 
						kund.rantfakt, round(trader.summa::numeric,2), 
							round((trader.momspliktigt*this_momsproc/100)::numeric,2),
							0, 0, trader.netto,
							order1.lagernr , order1.direktlevnr , this_momsproc, null, null
					from fuppg, order1 , (select sum(summa) as summa, sum(case when momsfri then o else summa end as momspliktigt), sum(netto) as netto from temprader) trader
					where order1.ordernr = in_orderrader[1];
				
	update faktura1 set t_orut =  round((t_netto+t_moms)::numeric,0)-(t_netto+t_moms) where faktnr=this_faktnr;
	update faktura1 set t_attbetala =  round((t_netto+t_moms+t_orut)::numeric,0) where faktnr=this_faktnr;
	select t_attbetala into this_tattbetala from faktura1 where faktnr=this_faktnr;  			


	-- lagersaldo
	insert into lager (lagernr, artnr, ilager) as l select (select this_lagernr, artnr, -sum(lev) as antal from temprader group by artnr)
	on conflict do 	update set l.ilager = l.ilager + excluded.ilager, l.iorder = l.iorder + excluded.ilager
					where l.lagernr=this_lagernr, l.artnr=excluded.artnr;

	insert into lagerhand (artnr, lagernr, datum, tid, anvandare, handelse, ordernr, stjid, gammaltilager, nyttilager, forandring, bestnr) 
		select artnr, this_lagernr, current_date, current_time, this_anvandare, 'Fakturerad', 0, 0, 0, 0, -sum(lev), 0 from temprader where artnr in (select nummer from artikel);

	-- uppdatera stärnrader
	update stjarnrad set fakturanr = this_faktnr where stjid in (select stjid from temprader where stjid>0 and artnr like '*%');
	

	-- Kundreskontra
	if this_tattbelata <> 0 then
		insert into kundres (rantfakt, faktnr, kundnr, namn, tot, netto, datum, falldat, faktor, bonus, medelmomsproc)
			select rantfakt, faktnr, kundnr, namn, t_attbetala, t_netto, datum, datum+ktid, faktor, bonus, t_moms::numeric / t_netto::numeric * 100 from faktura1 where faktnr=this_faktnr;

		if (select faktor from faktura1 where faktnr=this_faktnr)) <> 0 then 
			insert into faktorut (faktnr, datum, tot, kundnr, namn, falldat)
				select faktnr, datum, tot, kundnr, namn, falldat from kundres where faktnr=this_faktnr;
		end if;
	else -- om det är nullfaktura så lägg direkt i betald
		insert into betjour (rantfakt, faktnr, kundnr, namn, bet, betdat, betsatt, bonsumma, tallopnr, ar, man, pantsatt, betsattkonto)
			select rantfakt, faktnr, kundnr, namn, 0, datum, 'K', 0, 0, year(datum), month(datum), 0, 0 from faktura1 where faktnr=this_faktnr;
	end if;

	
	-- Artikelstatistik
	insert into artstat as a (artnr, ar, man, salda, tbidrag) 
		select artnr, year(datum), month(datum), lev, summa-netto*lev from faktura2 where faktnr=this_faktnr and artnr in (select nummer from artikel)
	on conflict do 	update set a.salda = a.salda+excluded.salda, a.tbidrag = a.tbidrag + excluded.tbidrag 
					where a.artnr=excluded.artnr and ar=excluded.ar and man=excluded.man;
	
	-- leverantörsstatistik
	insert into levstat as ls (levnr, ar, man, ftot, ftbidrag)
		select a.lev as levnr, year(current_date), month(current_date), sum(f2.summa) as summa, sum(f2.summa-f2.lev*f2.netto) as tb from faktura2 f2 join artikel a on a.nummer=f2.artnr join lev l on l.nummer=a.lev where trim(coalesce(a.lev,'')) <> ''  and f2.faktnr=this_faktnr group by a.lev
	on conflict do 	update set ls.ftot = ls.ftot + = excluded.ftot, ls.ftbidrag = ls.ftbidrag+excluded.ftbidrag 
					where ls.levnr=excluded.levnr and ls.ar=excluded.ar and ls.man=excluded.man;

	-- säljarestatistik
	insert into sljstat as ss (saljare, ar, man, totalt, tbidrag) 
		select trim(substring(saljare, 1, 30)), year(datum), month(datum), t_netto, t_netto-t_innetto from faktura1 where faktnr=this_faktnr and trim(substring(saljare, 1, 30)) in (select namn from saljare) 
	on comflict do	update set ss.totalt = ss.totalt+excluded.totalt, ss.tbidrag  = ss.tbidrag+excluded.tbidrag
					where ss.namn = excluded.namn and ss.ar=excluded.ar and ss.man=excluded.man ;
	
	-- statistik
	
	insert into statistik as ss 	(ar, manm fak_netto, fak_moms, fak_attbetala, fak_innetto, fak_antal, 
									fak_rantatot, dak_rantaantal, kun_betalt, kun_betaltant, 
									kun_brhog, kun_brhogant, kun_brlag, kun_brlagant, 
									kun_bejreg, kun_bejregant, kun_rankost, lev_attbet, lev_attbetant, 
									lev_ranta, lev_rantaant, lev_varu, lev_varuantal, lev_betalt, lev_betaltant )
			select 	year(datum), man(datum), t_netto, t_moms, t_attbetala, t_innetto, 1,
					0, 0, 0, 0,
					0, 0, 0, 0,
					0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0
				from faktura1 where faktnr=this_faktnr
	on conflict do 	update set 	ss.fak_netto=ss.fak_netto+excluded.fak_netto, ss.fak_moms=ss.fak_moms+rxcluded.fak_moms, 
								ss.fak_attbetala=ss.fak_attbetala0excluded.fak_attbetala, ss.dak_innetto=ss.fak_innetto+excluded.fak_innetto, ss.fak_antal=ss.fak_antal+1
					where ss.ar=excluded.ar and ss.man=excluded.man;
	
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
			from order1 where ordernr in(in_orderlist);
			
	
	
	

		
-- set orderhändelse fakturerad
	insert into orderhand (ordernr, datum, tid, anvandare, handelse, nyordernr, antalkolli, totalvikt) 
		select ordernr, current_date, current_time, in_anvandare, 'Fakturerad', 0, 0, 0 from unnest(in_orderlist) as ordernr;

-- delette ordewr
	delete from order1 where ordernr in (in_orderlist);
	delete from order2 where ordernr in (in_orderlist);


end;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

