#!/usr/bin/bash

echo "Loading test.postgres-1.sql into PostgreSQL (creating the sql_bridge_test database)"
sudo -u postgres psql < test.postgres-1.sql
echo "Loading test.postgres-2.sql into PostgreSQL (in the sql_bridge_test database)"
sudo -u postgres psql sql_bridge_test < test.postgres-2.sql
