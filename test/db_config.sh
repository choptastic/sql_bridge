#!/bin/bash

PGCMD=/usr/bin/psql
MYSQLCMD=/usr/bin/mysql

if [[ -z $GITHUB_ACTION ]]; then
	## We're not in a github action, so we're going to use the ubuntu
	## out-of-the-box configuration to connect to postgres and mysql
	## with sudo.
	## This could be reworked to install a local docker image, then remove it
	## after the fact, but for now, this is the method we're going to use
	PGCMD="sudo -u postgres $PGCMD -q"
	MYSQLCMD="sudo $MYSQLCMD"
fi

echo "[*] Initializing the PostreSQL database"
$PGCMD < test/postgres/pg-test-1.sql
$PGCMD sql_bridge_test < test/postgres/pg-test-2.sql
echo "[*] Initializing the MySQL database"
$MYSQLCMD -u root < test/mysql/test.sql
