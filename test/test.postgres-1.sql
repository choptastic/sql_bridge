
drop database if exists sql_bridge_test;
drop user if exists sql_bridge_user;

create database sql_bridge_test;
create user sql_bridge_user with login password 'sql_bridge_test_password';

grant all on database sql_bridge_test to sql_bridge_user;
