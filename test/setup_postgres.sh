#!/bin/sh

sudo -u postgres psql < test.postgres-1.sql
sudo -u postgres psql sql_bridge_test < test.postgres-2.sql
