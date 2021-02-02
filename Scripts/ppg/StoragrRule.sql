CREATE TABLE ppgexportartikelstoragerule (
	id serial NOT NULL,
	dateerror varchar NULL,
	errormessage varchar NULL,
	crdt date NULL DEFAULT CURRENT_DATE,
	status int4 NULL,
	materialname varchar not null ,	
	binname varchar not NULL,
	isdefaultbin boolean not null default false,
	materialcommand varchar not null default 'Save',
	CONSTRAINT ppgexportartikelstoragerule_pkey PRIMARY KEY (id)
);

grant all on ppgexportartikelstoragerule to ppg;


CREATE OR REPLACE FUNCTION ppgaddstoragerule(artnr varchar, rulename varchar, maxantal integer, isdefaultbin boolean default false ) returns void AS
$BODY$
begin
	
end;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;
