#!/bin/bash

PGCMD=/usr/bin/psql
MYSQLCMD=/usr/bin/mysql


if [[ -z $GITHUB_ACTION && -z $PGHOST && -z $MYSQ_HOST ]]; then
    . test/db.env
fi

echo "[*] Provisioning the Postres database server"
$PGCMD < test/postgres/pg-test-1.sql
$PGCMD sql_bridge_test < test/postgres/pg-test-2.sql
echo "[*] Provisioning the database MySQL databse server"
$MYSQLCMD -u root < test/mysql/test.sql
