drop user if exists sql_bridge_user@'%';
create user 'sql_bridge_user'@'%' identified by 'sql_bridge_test_password';
grant all privileges on sql_bridge_test.* to 'sql_bridge_user'@'%';
