select count(*) over (partition by t.artnr), T.ZONENAME, t.locationname, t.binname, t.artnr, a.namn, t.antal, t.maxantal , f.fakt
, sum(t.antal) over (partition by t.artnr) as totlager
from tempppglager t 
left outer join artikel a on a.nummer = t.artnr
left outer join lager l on l.artnr=a.nummer and lagernr = 0
left outer join ( select f2.artnr, sum(f2.lev) as fakt from faktura1 f1 join faktura2 f2 on f1.faktnr =f2.faktnr where f1.datum > current_date-365 and f1.lagernr=0 group by artnr) f
on f.artnr=a.nummer 
order by count(*) over (partition by t.artnr) desc, t.artnr



insert into ppgorderexporttest (status, directiontype, ordernr, kund, artnr, best, pos, picklocation, putlocation, putbin, priority) values (
		0,2,'TEST', 'INTERN', 'Q001', 3, 3, 'A=Pallställ|A=Automat', null, null, 2)
		
create table ppgorderexporttest as select * from ppgorderexport limit 1

alter table ppgorderexporttest add column id serial primary key

ALTER TABLE sxfakt.ppgorderexporttest ADD CONSTRAINT ppgorderexporttest_pkey PRIMARY KEY (id)


alter table orderhand drop constraint orderhand_pkey

alter table orderhand add column id serial primary key


delete from ppgorderexporttest 

grant all on ppgorderexporttest to ppg
