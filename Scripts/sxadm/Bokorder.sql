create view v_bokord as

select konto as konto,  f1.faktnr as faktnr, 'F' as typ, 
round(
(case when konto = 'Orut' then 1 else 0 end * f1.t_orut +
case when konto = 'AttBetala' then 1 else 0 end * f1.t_attbetala +
case when konto = 'Moms 1' and f1.moms=1 then 1 else 0 end * -f1.t_moms +
case when konto = 'Moms 2' and f1.moms=2 then 1 else 0 end * -f1.t_moms +
case when konto = 'Moms 3' and f1.moms=3 then 1 else 0 end * -f1.t_moms )::numeric
,2)
as summa, 
f1.kundnr as kundnr, f1.namn as namn, f1.datum as datum, year(f1.datum) as ar, month(f1.datum) as man
from faktura1 f1 join (values ('Moms 1'),('Moms 2'),('Moms 3'),('AttBetala'), ('Orut')) k (konto) on 1=1


union all
select case when coalesce(f2.konto,'') = '' then '3011' else f2.konto end, f2.faktnr , 'F', -sum(round(f2.summa::numeric,2)) , f1.kundnr, f1.namn, f1.datum as datum, year(f1.datum) as ar, month(f1.datum) as man
from faktura1 f1 join faktura2 f2 on f1.faktnr = f2.faktnr 
group by f1.faktnr, f2.faktnr, case when coalesce(f2.konto,'') = '' then '3011' else f2.konto end
union all 
select case when k.typ='B' then 'Betalning ' || b.betsatt else 'Kundreskontra' end, b.faktnr, 'B', 
round(b.bet::numeric,2) * case when k.typ='B' then 1 else -1 end as summa, 
b.kundnr, b.namn, b.betdat, year(b.betdat), month(b.betdat)
from betjour b join (values ('B'), ('R')) k (typ) on 1=1

