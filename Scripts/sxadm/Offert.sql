
CREATE OR REPLACE FUNCTION offert1add(
    in_anvandare character varying,
    in_lagernr integer,
    in_kundnr character varying,
    in_marke character varying)
  RETURNS integer AS
$BODY$
declare
	this_offertnr integer;
begin
	
	if not exists (select 1 from lagerid where lagerid.lagernr=in_lagernr) then raise exception 'Lagernummer % saknas', in_lagernr; end if;
	if not exists (select 1 from saljare where forkortning=in_anvandare) then raise exception 'Användare % saknas', in_anvandare; end if;
	if not exists (select 1 from kund where nummer=in_kundnr) then raise exception 'Kundnummer % saknas', in_kundnr; end if;
	
	select offertnr into this_offertnr from faktdat;
	update faktdat set offertnr = offertnr+1;
	insert into offert1 (offertnr, namn, adr1, adr2, adr3, levadr1, levadr2, levadr3, 
		saljare, referens, kundnr, marke, datum, moms, status, ktid, bonus, faktor, levdat, 
		levvillkor, mottagarfrakt, fraktkundnr, fraktbolag, fraktfrigrans, lagernr,
		skrivejpris
	)
		select this_offertnr, namn, adr1, adr2, adr3, lnamn, ladr2, ladr3,
		saljare, ref, nummer, in_marke, current_date, case when momsfri = 0 then 1 else 0 end, 'Sparad', ktid, bonus, faktor, null,
		levvillkor, mottagarfrakt, fraktkundnr, fraktbolag, fraktfrigrans, in_lagernr,
		0
		from kund where nummer=in_kundnr;
	return this_offertnr;
end;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

 
 
 
 
 
 
 
 
