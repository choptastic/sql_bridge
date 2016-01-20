grant all on database sql_bridge_test to sql_bridge_user;
alter default privileges
	in schema public
	grant select, insert, update, delete on tables to sql_bridge_user;

ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT, USAGE ON sequences TO sql_bridge_user;

create table fruit (
	fruitid serial primary key,
	fruit varchar(50),
	description text,
	quantity integer,
	picture bytea
);
