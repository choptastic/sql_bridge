drop database if exists sql_bridge_test;
create database sql_bridge_test;
use sql_bridge_test;
grant all on sql_bridge_test.* to 'sql_bridge_user'@'%' identified by 'sql_bridge_test_password';

create table fruit (
	fruitid int auto_increment primary key,
	fruit varchar(50),
	description text,
	quantity integer,
	picture blob,
	some_float float
) character set utf8 engine=innodb;
