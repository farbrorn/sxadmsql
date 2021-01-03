CREATE OR REPLACE FUNCTION sxasFaktureraOffert(in_anvandare varchar, in_offertnr integer, in_fraktkostnad float)   
RETURNS integer AS
$BODY$
declare 
	this_ordernr integer;
	this_faktnr integer;
	temp_pos integer;
begin
	if not exists (select from offert1 where offertnr=in_offertnr and kundnr='00478') then raise EXCEPTION 'Offert % på kundnr 00478 hittades inte.', in_offertnr; END IF;
	select 	orderfromoffert('00',in_offertnr, 'Temp') into this_ordernr;
	if in_fraktkostnad > 0 then
		select orderaddrow_noorderhand('00', this_ordernr, '0020', 1, in_fraktkostnad, 0) into temp_pos;
		update order2 set netto=in_fraktkostnad where ordernr=this_ordernr and pos=temp_pos;
	end if;

	select faktureraorder('00', array [this_ordernr]) into this_faktnr;
	return this_faktnr;
end;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;


