drop database if exists sql_bridge_test;
create database sql_bridge_test;

use sql_bridge_test;

drop table if exists fruit;
create table fruit (
	  fruitid int auto_increment primary key,
	  fruit varchar(50),
	  description text,
	  quantity integer,
	  picture blob,
	  some_float float
) character set utf8 engine=innodb;

drop table if exists other;
create table other (
	otherid int auto_increment primary key,
	my_time time,
	my_date date,
	my_decimal decimal(6,2),
	my_datetime datetime,
    si smallint,
    bi bigint,
    vc varchar(20),
    c char(20),
    u uuid
);

-- testing IDs that aren't tablename + 'id'
drop table if exists other_auto;
create table other_auto (
    id int auto_increment primary key,
    some_text text
);

-- testing IDs that don't auto_increment
drop table if exists other_int;
create table other_int (
    id int unsigned primary key,
    some_text text
);

drop table if exists other_string;
create table other_string (
    id varchar(25) primary key,
    some_text text
);

drop table if exists other_uuid;
create table other_uuid (
    id uuid primary key,
    some_text text
);
