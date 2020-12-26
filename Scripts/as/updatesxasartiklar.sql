-- Function: updatesxasartiklar(real)

-- DROP FUNCTION updatesxasartiklar(real);

CREATE OR REPLACE FUNCTION updatesxasartiklar(in_prisfaktor2 real DEFAULT 0)
  RETURNS void AS
$BODY$
declare
this_prisfaktor2 real;
begin
if (in_prisfaktor2 = 0) then this_prisfaktor2 = 1.07; else this_prisfaktor2=in_prisfaktor2; end if;

if (this_prisfaktor2 < 0.5) then raise exception 'Prisfaktorn är för låg. (%)', this_prisfaktor2; end if;
if (this_prisfaktor2 > 5) then raise exception 'Prisfaktorn är för hög. (%)', this_prisfaktor2; end if;

create temporary table tempart on commit drop as select * from sxfakt.artikel a where  a.nummer not in (select nummer from sxasfakt.artikel);
insert into sxasfakt.artikel select * from tempart;
update sxasfakt.artikel set lev = 'SX', utrab = case when utrab = 40 then 45 else utrab end , rsk=''  where nummer in (select nummer from tempart);

create temporary table tempvaluta on commit drop as select coalesce(kurs,0) as nok from sxfakt.valuta where valuta='NOK';

update sxasfakt.artikel a set 
utpris = (select round((utpris*case when utrab <=5 then (select (1/nok)::numeric from tempvaluta) else 1.1*this_prisfaktor2 end)::numeric,case when utpris > 60 then 0 else case when utpris > 20 then 1 else 2 end end) from sxfakt.artikel aa where aa.nummer=a.nummer),
inpris = (select round((inpris/(select nok from tempvaluta))::numeric,2) from sxfakt.artikel aa where aa.nummer=a.nummer),
staf_pris1 = (select round((staf_pris1*case when utrab <=5 then (select 1/nok*1.1 from tempvaluta) else 1.1 end)::numeric,case when staf_pris1 > 60 then 0 else case when staf_pris1 > 20 then 1 else 2 end end) from sxfakt.artikel aa where aa.nummer=a.nummer),
staf_pris2 = (select round((staf_pris2*case when utrab <=5 then (select 1/nok*1.1 from tempvaluta) else 1.1 end)::numeric,case when staf_pris2 > 60 then 0 else case when staf_pris2 > 20 then 1 else 2 end end) from sxfakt.artikel aa where aa.nummer=a.nummer),
staf_antal1 = (select staf_antal1 from sxfakt.artikel aa where aa.nummer=a.nummer),
staf_antal2 = (select staf_antal2 from sxfakt.artikel aa where aa.nummer=a.nummer),
utgattdatum = (select utgattdatum from sxfakt.artikel aa where aa.nummer=a.nummer),
inpdat = (select inpdat from sxfakt.artikel aa where aa.nummer=a.nummer),
prisdatum = (select prisdatum from sxfakt.artikel aa where aa.nummer=a.nummer),
inp_frakt = (select inp_frakt from sxfakt.artikel aa where aa.nummer=a.nummer),
inp_fraktproc = (select inp_fraktproc from sxfakt.artikel aa where aa.nummer=a.nummer),
inp_miljo = (select inp_miljo from sxfakt.artikel aa where aa.nummer=a.nummer),
enhet = (select enhet from sxfakt.artikel aa where aa.nummer=a.nummer),
rab = (select rab from sxfakt.artikel aa where aa.nummer=a.nummer),
utrab = (select case when utrab=40 then 45 else utrab end from sxfakt.artikel aa where aa.nummer=a.nummer),
rabkod = (select rabkod from sxfakt.artikel aa where aa.nummer=a.nummer),
kod1 = (select kod1 from sxfakt.artikel aa where aa.nummer=a.nummer),
vikt = (select vikt from sxfakt.artikel aa where aa.nummer=a.nummer),
volym = (select volym from sxfakt.artikel aa where aa.nummer=a.nummer),
struktnr = (select struktnr from sxfakt.artikel aa where aa.nummer=a.nummer),
forpack = (select forpack from sxfakt.artikel aa where aa.nummer=a.nummer),
kop_pack = (select kop_pack from sxfakt.artikel aa where aa.nummer=a.nummer),
cn8 = (select cn8 from sxfakt.artikel aa where aa.nummer=a.nummer),
fraktvillkor = (select fraktvillkor from sxfakt.artikel aa where aa.nummer=a.nummer),
dagspris = (select dagspris from sxfakt.artikel aa where aa.nummer=a.nummer),
hindraexport = (select hindraexport from sxfakt.artikel aa where aa.nummer=a.nummer),
minsaljpack = (select minsaljpack from sxfakt.artikel aa where aa.nummer=a.nummer),
storpack = (select storpack from sxfakt.artikel aa where aa.nummer=a.nummer),
prisgiltighetstid = (select prisgiltighetstid from sxfakt.artikel aa where aa.nummer=a.nummer),
bildartnr = (select bildartnr from sxfakt.artikel aa where aa.nummer=a.nummer)
where a.nummer in (select nummer from sxfakt.artikel) and a.lev='SX' ;
delete from sxfakt.nettopri where lista='SXAS';
insert into sxfakt.nettopri (select 'SXAS', nummer, ROUND(CAST((inpris*(1-rab/100)*(1+inp_fraktproc/100)+inp_frakt+inp_miljo)/case when nummer like 'XN*%' then 0.95 else 0.84 end as numeric),2), '', current_date from sxfakt.artikel 
where inpris>0 );

--uppdatera nettorpislistan till AB
delete from sxasfakt.nettopri where lista='NETTO0';
insert into sxasfakt.nettopri (select 'NETTO0', nummer, ROUND(CAST((inpris*(1-rab/100)*(1+inp_fraktproc/100)+inp_frakt+inp_miljo) as numeric),2), '', current_date from sxasfakt.artikel 
where inpris>0 );
                    
end
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION updatesxasartiklar(real)
  OWNER TO sxfakt;
