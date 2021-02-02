--drop FUNCTION sxasOverforOrder2Offert(in_orderlista integer[])


CREATE OR REPLACE FUNCTION sxasOverforOrder2Offert(in_orderlista integer[], out out_offertnr integer, out out_viktkg integer, out out_antalkolli integer)  AS
$BODY$
declare
	this_offertnr integer;
	this_antalkolli integer;
	this_viktkg integer;
	this_meddelande varchar;
	this_iserror boolean;
	this_on integer;
begin
	--Kolla efter dubbla order
	select  ordernr into this_on from unnest(in_orderlista::integer[]) as ordernr group by ordernr  having count(*) > 1 limit 1;
	if this_on > 0 then raise exception 'Ordernr % är dubbelt angiven.', this_on; end if;
	--Kolla efter felaktiga ordrar samt räkna vikt 
	select 
	string_agg( ' Order ' || o.ordernr ||
	case when ejsamfak+stjarnrader+saknadeartiklar+kreditrader+saknarinprisrader > 0 then 
		case when ejsamfak > 0 then  ' - Ej på samfakt eller saknas. ' else '' end ||
		case when  stjarnrader > 0 then  ' - Innehåller icke tillåtna *-rader. ' else '' end ||
		case when  saknadeartiklar > 0 then  ' - Innehåller rader som saknas i artikleregistret. ' else '' end ||
		case when  kreditrader > 0 then  ' - Innehåller kreditrader. ' else '' end ||
		case when  saknarinprisrader > 0 then  ' - Saknar inköpspris i artikelregistret. ' else '' end 
	else ' - OK' end 
	, '<br>') as meddelande 
	, sum(ejsamfak+stjarnrader+saknadeartiklar+kreditrader+saknarinprisrader) > 0 as iserror 
	, sum(antalkolli) as antalkolli 
	, sum(viktkg) as viktkg 
	into this_meddelande, this_iserror, this_antalkolli, this_viktkg
	from (
		select olist.ordernr 
		, case when o1.status='Samfak' then 0 else 1 end as ejsamfak
		, sum(case when o2.artnr like '*%' then 1 else 0 end)  as stjarnrader
		, sum(case when a.nummer is null then 1 else 0 end)  as saknadeartiklar
		, sum(case when o2.lev < 0 then 1 else 0 end)  as kreditrader
		, sum(case when a.getinnetto <=0 then 1 else 0 end)  as saknarinprisrader
		, max(kol.antalkolli) as antalkolli
		, max(kol.viktkg) as viktkg
		from
		(select unnest(in_orderlista) as ordernr) olist 
		left outer join sxasfakt.order1 o1 on o1.ordernr=olist.ordernr 
		left outer join sxasfakt.order2 o2 on o1.ordernr=o2.ordernr and trim(coalesce(o2.artnr,'')) <> ''
		left outer join artikel a on a.nummer=o2.artnr
		left outer join (select wmsordernr, count(*) as antalkolli, sum(viktkg) as viktkg from sxfakt.wmskollin group by wmsordernr) kol on kol.wmsordernr= 'AS-' || o1.ordernr
		group by olist.ordernr, o1.ordernr
	) o; 
	
	-- Fel på någon order
	if this_iserror then raise exception '%', this_meddelande; end if;


	--Skapa offert	
	select offert1add('00',0,'00478','') into this_offertnr ;

	insert into offert2 (offertnr, pos, prisnr, artnr, namn, levnr, best, rab, lev, text, pris, summa, konto, netto, enh)
	select this_offertnr, row_number() over (), 1, o2.artnr, a.namn, a.lev, o2.lev, 0, o2.lev, ''
	, 	round(
			case when a.getinnetto <= 0 then 
				case when o2.pris >= 0 then 
					o2.pris * (1-coalesce(o2.rab,0)/100) * valuta.kurs 
				else 
					utpris 
				end 
			else 
				-- multiplikator för beh¨llning i ab
				a.getinnetto + greatest(0,greatest(0,(o2.pris * (1-coalesce(o2.rab,0)/100) * valuta.kurs) - a.getinnetto)) * 0.44 
			end::numeric 
		,2) as pristillas
	, 0, '', a.getinnetto, a.enhet
	from
	(select unnest(in_orderlista) as ordernr) olist 
	join sxasfakt.order1 o1 on o1.ordernr=olist.ordernr 
	join sxasfakt.order2 o2 on o1.ordernr=o2.ordernr and trim(coalesce(o2.artnr,'')) <> ''
	join artikel a on a.nummer=o2.artnr
	join valuta on valuta.valuta='NOK'
	where o2.artnr not like '*%' and o2.lev > 0;
	
	update offert2 set summa=round((lev*pris*(1-rab/100))::numeric,2) where offertnr=this_offertnr;

	out_offertnr=this_offertnr;
	out_viktkg = this_viktkg;
	out_antalkolli=this_antalkolli;
end;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

 
 
