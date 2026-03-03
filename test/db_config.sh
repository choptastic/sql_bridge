#!/bin/bash

compare_versions() {
    #if [ $# -ne 2 ]; then
    #    echo "Usage: compare_versions <version1> <version2>" >&2
    #    return 2
    #fi

    if printf '%s\n%s\n' "$1" "$2" | sort -V -c 2>/dev/null; then
        echo "$1 <= $2"
        return 0
    else
        echo "$1 > $2"
        return 1
    fi
}

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
$PGCMD <test/postgres/pg-test-1.sql
echo "[*] Populating the PostreSQL database"
$PGCMD sql_bridge_test <test/postgres/pg-test-2.sql
echo "[*] Creating and populating the MySQL database"
$MYSQLCMD <test/mysql/create-database.sql

echo "[*] Checking MySQL Version"
MY_VSN=$($MYSQLCMD -B -e "select version()" | grep "[0-9]")
echo "[*] MySQL Version: $MY_VSN"
IS_MARIA=$(echo $MY_VSN | grep -i mariadb)
if [[ -n $IS_MARIA ]]; then
    echo "[*] MySQL is MariaDB"
    if [[ $(compare_versions "10.6.99999" "$MY_VSN") ]]; then
        echo "[*] MariaDB Version is 10.7+. Enabling UUID Field Types"
        $MYSQLCMD <test/mysql/uuid.sql
    else
        echo "[ ] MariaDB is not 10.7+. Skipping UUID Field Types"
    fi
else
    echo "[ ] MySQL is NOT MariaDB. Skipping UUID Type Fields."
fi

if [[ -z $GITHUB_ACTION ]]; then
    ## we only initialize the user if we're not running this from a github action.
    ## This is because the workflows file in .github/ is manually creating the
    ## users
    echo "[*] Initializing the MySQL test user"
    $MYSQLCMD <test/mysql/init-user.sql
fi
