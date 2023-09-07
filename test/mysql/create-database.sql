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
	my_datetime datetime
);
