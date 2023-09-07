#!/bin/bash

PGCMD="/usr/bin/psql -q"
MYSQLCMD="/usr/bin/mysql --verbose"

if [[ -z $GITHUB_ACTION ]]; then
	## We're not in a github action, so we're going to use the ubuntu
	## out-of-the-box configuration to connect to postgres and mysql
	## with sudo.
	## This could be reworked to install a local docker image, then remove it
	## after the fact, but for now, this is the method we're going to use
	PGCMD="sudo -u postgres $PGCMD"
	MYSQLCMD="sudo $MYSQLCMD"
else
	MYSQLCMD="$MYSQLCMD -uroot"
fi


echo "[*] Initializing the PostreSQL database and user"
$PGCMD < test/postgres/pg-test-1.sql
echo "[*] Populating the PostreSQL database"
$PGCMD sql_bridge_test < test/postgres/pg-test-2.sql
echo "[*] Creating and populating the MySQL database"
$MYSQLCMD < test/mysql/create-database.sql

if [[ -z $GITHUB_ACTION ]]; then
	## we only initialize the user if we're not running this from a github action.
	## This is because the workflows file in .github/ is manually creating the
	## users
	echo "[*] Initializing the MySQL test user"
	$MYSQLCMD < test/mysql/init-user.sql
fi
