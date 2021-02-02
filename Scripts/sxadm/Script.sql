select A.NUMMER, A.NAMN, A.GETINNETTO*L.FORANDRING
FROM ARTIKEL A join LAGERHAND L on L.ARTNR=A.NUMMER and L.LAGERNR=0
where A.NUMMER like 'SB%' and L.DATUM='2021-01-22' and L.HANDELSE='Korrigering'

select * from  sxasfakt.sxservjobb where jobbid=6267

select * from update sxasfakt.sxreg set varde='kli' where id='Hemsida-ProduktView'