CREATE OR REPLACE FUNCTION sxasOverforLagersaldo()  returns void as 
$BODY$
begin
	
delete from sxfakt.rorder where kundnr='Y0001';

insert into sxfakt.rorder (kundnr, artnr, id, namn, pris, rab, rest, enh, konto, netto, marke, levnr, levdat, levbestdat, lagernr, stjid)
	select 'Y0001', a.nummer, 1, a.namn, 0, 0, sum(o2.best), a.enhet, '', 0, '', a.lev, null, null, 0, 0
		from sxasfakt.order1 o1 join sxasfakt.order2 o2 on o1.ordernr=o2.ordernr join sxasfakt.artikel a on a.nummer=o2.artnr --join sxasfakt.kund k on k.nummer=o1.kundnr
		where o1.status in ('Sparad','Utskr') and o1.kundnr <> '055561610'
		group by a.nummer;


create temporary table o on commit drop as 
	select lagernr, artnr, sum(iorder) as iorder, sum(ibest) as ibest from (
		select o1.lagernr as lagernr, o2.artnr as artnr, sum(o2.best) as iorder, 0 as ibest 
			from sxfakt.order1 o1 
			join sxfakt.order2 o2 on o1.ordernr=o2.ordernr 
			where o1.lagernr=0 
			group by o1.lagernr, o2.artnr
	
		union 
		
		select lagernr, artnr, sum(rest) as iorder, 0 as ibest 
			from sxfakt.rorder 
			where lagernr=0 
			group by lagernr, artnr
		
		union
		
		select b1.lagernr, b2.artnr, 0 as iorder, sum(b2.best) as ibest  
			from sxfakt.best1 b1 
			join sxfakt.best2 b2 on b1.bestnr=b2.bestnr 
			group by b1.lagernr, b2.artnr
	
	) bbb join sxfakt.artikel a on a.nummer=bbb.artnr where lagernr=0 group by lagernr, artnr;



insert into sxfakt.lager (artnr, lagernr, ilager, bestpunkt, maxlager, best, iorder, lagerplats, hindrafilialbest) 
	select o.artnr, o.lagernr, 0, 0, 0, o.ibest, o.iorder, '', 0  
		from o 
		where not exists 
			(select * from sxfakt.lager l where l.lagernr=o.lagernr and l.artnr=o.artnr); 

update sxfakt.lager set iorder=0, best=0 where lagernr=0;


update sxfakt.lager l set iorder=o.iorder, best=o.ibest
	from o where l.lagernr=o.lagernr  and l.artnr = o.artnr;

end;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;
