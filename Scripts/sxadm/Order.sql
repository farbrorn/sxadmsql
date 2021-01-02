CREATE OR REPLACE FUNCTION orderadd(in_anvandare character varying, in_lagernr integer, in_kundnr character varying, in_marke character varying, in_artnr character varying[], in_antal real[], in_pris real[], in_rab real[])
 RETURNS integer
 LANGUAGE plpgsql
 SECURITY DEFINER
AS $function$
declare
	this_ordernr integer;
	this_cn integer;
begin
	
	if not exists (select 1 from lagerid where lagerid.lagernr=in_lagernr) then raise exception 'Lagernummer % saknas', in_lagernr; end if;
	if not exists (select 1 from saljare where forkortning=in_anvandare) then raise exception 'Användare % saknas', in_anvandare; end if;
	if not exists (select 1 from kund where nummer=in_kundnr) then raise exception 'Kundnummer % saknas', in_kundnr; end if;
	
	select fdordernr.ordernr into this_ordernr from fdordernr;
	update fdordernr set ordernr = ordernr+1;
	insert into order1 (ordernr, dellev, namn, adr1, adr2, adr3, levadr1, levadr2, levadr3, 
		saljare, referens, kundnr, marke, datum, moms, status, ktid, bonus, faktor, levdat, 
		levvillkor, mottagarfrakt, fraktkundnr, fraktbolag, fraktfrigrans, lagernr, 
		direktlevnr, returorder, veckolevdag, tillannanfilial, utlevbokad, annanlevadress, wordernr, 
		linjenr1, linjenr2, linjenr3)
		select this_ordernr, 1, namn, adr1, adr2, adr3, lnamn, ladr2, ladr3,
		saljare, ref, nummer, in_marke, current_date, case when momsfri <> 0 then 1 else 0 end, 'Sparad', ktid, bonus, faktor, null,
		levvillkor, mottagarfrakt, fraktkundnr, fraktbolag, fraktfrigrans, in_lagernr, 
		0, 0, 0, 0, 0, 0, 0,
		coalesce(linjenr1,''), coalesce(linjenr2,''), coalesce(linjenr3,'')
		from kund where nummer=in_kundnr;

	insert into orderhand (ordernr, datum, tid, anvandare, handelse, nyordernr, antalkolli, totalvikt) values (this_ordernr, current_date, current_time, in_anvandare, 'Skapad', 0, 0, 0); 


for this_cn in 1..array_upper(in_artnr,1) loop
	if not exists (select 1 from artikel where nummer = in_artnr[this_cn]) then raise exception 'Artikel % finns inte', in_artnr[this_cn]; end if;
	if in_antal[this_cn] is null then raise exception 'Antal för artikel % är null', in_artnr[this_cn]; end if;
	if in_pris[this_cn] is null then raise exception 'Pris för artikel % är null', in_artnr[this_cn]; end if;
	insert into order2 (ordernr, pos, prisnr, dellev, artnr, namn, levnr, best, rab, lev, pris, summa, konto, netto, enh, stjid)
		select this_ordernr, this_cn, 1, 1, nummer, namn, lev, in_antal[this_cn] , coalesce(in_rab[this_cn],0), in_antal[this_cn], in_pris[this_cn], round((in_pris[this_cn]*in_antal[this_cn]*(1-coalesce(in_rab[this_cn],0)/100))::numeric,2) , konto, round((inpris*(1-rab/100)*(1+inp_fraktproc/100)+inp_frakt+inp_miljo)::numeric,2), enhet, 0
		from artikel where nummer=in_artnr[this_cn];
	if not exists (select 1 from lager where artnr = in_artnr[this_cn] and lagernr = in_lagernr) then insert into lager (artnr, lagernr, ilager, bestpunkt, maxlager, best, iorder, hindrafilialbest) values (in_artnr[this_cn], in_lagernr, 0,0,0,0,0,0); end if;
	update lager set iorder=iorder+in_antal[this_cn] where artnr=in_artnr[this_cn] and lagernr=in_lagernr;
end loop; 


	return this_ordernr;
end;
$function$
;










CREATE OR REPLACE FUNCTION orderaddrow(in_anvandare character varying, in_ordernr integer, in_artnr character varying, in_antal real, in_pris real DEFAULT NULL::real, in_rab real DEFAULT NULL::REAL, in_sparaorderhand boolean DEFAULT true)
 RETURNS integer
 LANGUAGE plpgsql
AS $function$
declare
	this_lagernr integer;
        this_pos integer;
begin
	if not exists (select from order1 where ordernr=in_ordernr) then raise exception 'Ordernummer % saknas', in_ordernr; end if;
	
	if not exists (select from saljare where forkortning=in_anvandare or in_anvandare='00') then raise exception 'Användare % saknas', in_anvandare; end if;
	if not exists (select from artikel where nummer = in_artnr) then raise exception 'Artikel % finns inte', in_artnr; end if;
	if in_antal is null then raise exception 'Antal för artikel % är null', in_artnr; end if;
	
	select into this_lagernr lagernr from order1 where ordernr=in_ordernr;
	
	IF in_sparaorderhand then	
		insert into orderhand (ordernr, datum, tid, anvandare, handelse, nyordernr, antalkolli, totalvikt) 
			values (in_ordernr, current_date, current_time, in_anvandare, 'Ny rad', 0, 0, 0); 
	END IF;

        select into this_pos coalesce(max(pos),0)+1 from order2 where ordernr=in_ordernr;
	insert into order2 (ordernr, pos, prisnr, dellev, artnr, namn, levnr, best, lev, pris, rab, summa, konto, netto, enh, stjid)
		select o1.ordernr, this_pos , 1, 1, a.nummer, a.namn, a.lev, in_antal , in_antal, 
		case when in_pris is not null then in_pris else least (
			case when kn.kundnetto_staf2>0 then  kn.kundnetto_staf2 else a.utpris end, 
			case when kn.kundnetto_staf1>0 then  kn.kundnetto_staf1 else a.utpris end,
			kn.kundnetto_bas
		) end,
		coalesce(in_rab,0), 
		
		case when in_pris is not null then in_pris else least (
			case when kn.kundnetto_staf2>0 then  kn.kundnetto_staf2 else a.utpris end, 
			case when kn.kundnetto_staf1>0 then  kn.kundnetto_staf1 else a.utpris end,
			kn.kundnetto_bas
		) end * in_antal *(1-coalesce(in_rab,0)/100), 
		
		konto, a.getinnetto, enhet, 0
		from artikel A JOIN order1 o1 on o1.ordernr=in_ordernr join kundnetto kn on kn.artnr=a.nummer and kn.kundnr=o1.kundnr 
		where a.nummer=in_artnr;
		
	if not exists (select from lager where artnr = in_artnr and lagernr = this_lagernr) then 
		insert into lager (artnr, lagernr, ilager, bestpunkt, maxlager, best, iorder, hindrafilialbest) 
			values (in_artnr, this_lagernr, 0,0,0,0,0,0); 
	end if;
	update lager set iorder=iorder+in_antal where artnr=in_artnr and lagernr=this_lagernr;
        return this_pos;
end;
$function$
;










CREATE OR REPLACE FUNCTION orderaddrow_noorderhand(in_anvandare character varying, in_ordernr integer, in_artnr character varying, in_antal real, in_pris real DEFAULT NULL::real, in_rab real DEFAULT NULL::real)
 RETURNS integer
 LANGUAGE plpgsql
AS $function$
declare
	this_lagernr integer;
        this_pos integer;
begin
        return orderaddrow(in_anvandare, in_ordernr, in_artnr, in_antal, in_pris, in_rab, FALSE);
end;
$function$
;










CREATE OR REPLACE FUNCTION orderFromOffert(in_anvandare character varying, in_offertnr integer, in_status varchar) RETURNS integer AS
$BODY$
declare
	this_ordernr integer;
	this_maxstjid integer;
	this_lagernr integer;
begin
	if not exists (select from saljare where forkortning=in_anvandare) then raise exception 'Användare % saknas', in_anvandare; end if;
	if not exists (select from offert1 where offertnr=in_offertnr) then raise exception 'Offert % saknas', in_offertnr; end if;
	if not exists (select from offert1 join kund on kund.nummer=offert1.kundnr where offertnr=in_offertnr) then raise exception 'Kund saknas på offert %', in_offertnr; end if;
	
	select fdordernr.ordernr into this_ordernr from fdordernr;
	update fdordernr set ordernr = ordernr+1;

	select lagernr into this_lagernr from offert1 where offertnr=in_offertnr;

	insert into order1 (ordernr, dellev, namn, adr1, adr2, adr3, levadr1, levadr2, levadr3, 
		saljare, referens, kundnr, marke, datum, moms, status, ktid, bonus, faktor, levdat, 
		levvillkor, mottagarfrakt, fraktkundnr, fraktbolag, fraktfrigrans, lagernr, 
		direktlevnr, returorder, veckolevdag, tillannanfilial, utlevbokad, annanlevadress, wordernr, 
		linjenr1, linjenr2, linjenr3)
		select this_ordernr, 1, offert1.namn , offert1.adr1 , offert1.adr2 , offert1.adr3 , offert1.levadr1 , offert1.levadr2, offert1.levadr3 ,
		offert1.saljare , offert1.referens , offert1.kundnr , offert1.marke , current_date, offert1.moms , in_status, offert1.ktid, offert1.bonus, offert1.faktor , offert1.levdat ,
		offert1.levvillkor , offert1.mottagarfrakt , offert1.fraktkundnr , offert1.fraktbolag , offert1.fraktfrigrans , offert1.lagernr,
		0, 0, kund.veckolevdag , 0, 0, offert1.annanlevadress , 0, coalesce(kund.linjenr1,'') , coalesce(kund.linjenr2,'') , coalesce(kund.linjenr3,'') 
		from	 offert1 join kund on kund.nummer=offert1.kundnr where offert1.offertnr=in_offertnr; 
	
	insert into order2 (ordernr, pos, prisnr, dellev, artnr, namn, levnr, best, rab, lev, pris, summa, konto, netto, enh, stjid)
		select this_ordernr, pos, offert2.prisnr , 1, artnr, namn, offert2.levnr , offert2.best , offert2.rab , offert2.lev, pris, summa, konto, offert2.netto , enh, 0
			from offert2 
			where offertnr=in_offertnr;

	-- stjh'rnrader
	-- Tilldelea först ett stjid i ordern, därefter skapa rad i stjarnrad
	select max(stjid) into this_maxstjid from stjarnrad;
	update order2 set stjid = q.stjid
		from (select pos, this_maxstjid+ row_number() OVER () as stjid from order2 where ordernr = this_ordernr and artnr like '*%' and coalesce(stjid,0)=0 ) q
		where order2.pos=q.pos and order2.ordernr=this_ordernr;

	insert into stjarnrad (stjid, artnr, levnr, kundnr, namn, lagernr, antal, enh, inpris, inrab, regdatum, autobestall, bestdat, bestnr, anvandare, finnsilager, inkomdatum, fakturanr)
		select stjid , artnr, levnr, kundnr, o2.namn, o1.lagernr, o2.lev, o2.enh , o2.netto , 0, current_date, 0, null, 0, in_anvandare, 0, null, 0
			from order1 o1 join order2 o2 on o1.ordernr=o2.ordernr where o1.ordernr=this_ordernr and o2.artnr like '*%' and coalesce(o2.stjid,0)>0;
	

	-- Lager
	insert into lager (artnr, lagernr, ilager, bestpunkt, maxlager, best, iorder, hindrafilialbest) 
		select artnr, this_lagernr, 0,0,0,0,0,0 from order2 join artikel on artikel.nummer=order2.artnr 
			where order2.ordernr=this_ordernr
				and artnr not in (select artnr from lager where lagernr=this_lagernr);
	

	update lager set iorder=lager.iorder + q.antal
		from (select order2.artnr, order1.lagernr, sum(order2.lev) as antal from order1 join order2 on order1.ordernr=order2.ordernr join artikel on artikel.nummer=order2.artnr where order1.ordernr=this_ordernr group by order2.artnr, order1.lagernr) as q
	where lager.artnr=q.artnr and lager.lagernr = q.lagernr;
		

	-- Orderhasnd
	insert into orderhand (ordernr, datum, tid, anvandare, handelse, nyordernr, antalkolli, totalvikt) 
		values (this_ordernr, current_date, current_time, in_anvandare, 'Skapad', 0, 0, 0); 

		
	return this_ordernr;
end;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;
