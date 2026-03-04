# SQL Bridge Changelog

## 0.7.0 (2026-03-04)

- The mysql-otp bridge has been reworked to use the mainline
  [mysql-otp](https://github.com/mysql-otp/mysql-otp) repo (rather than
  @choptastic's fork). This requires
  [new functionality](https://github.com/mysql-otp/mysql-otp/pull/194) in
  mysql-otp.
- The epgsql bridge now relies on mainline
  [epgsql](https://github.com/epgsql/epgsql) (rather than @choptastic's
  fork). This ended up requiring migrating @choptastic's epgsql customizations
  to become epgsql [custom
  codecs](https://github.com/epgsql/epgsql/blob/devel/doc/pluggable_types.md)
  and [epgsql_decimal](https://github.com/egobrain/epgsql_decimal).
- Added a primary key generator for SQL Bridge to use if the table is not using
  its own key generation. This key generator can be completely swapped out if
  its conditions are not sufficient for your needs.
- Added `primary_key(Table)` which returns the field name of the primary key of the
  provided table
- Added `is_auto_increment(Table, Field)` which returns true if the `Field` in
  `Table` is using the RDBMS's auto increment feature for primary key generation
  (ala MySQL's `auto_increment` or PostgreSQL's `serial`).
- Added `field_type(Table, Field)` which returns a limited arrangement of terms
  usable for key generation (that is, integers, strings, and UUIDs). Other field
  types are ignored (floats, dates, etc). This will likely be built out further
  eventually, but its current implementation is to go along with the new primary
  key generator.
- Fixed `epgsql`'s decoding of `decimal`/`numeric` types to return a float or
  integer. (Fixes [Issue #5](https://github.com/choptastic/sql_bridge/issues/5))
- Fixed a crash in the `epgsql` driver
  ([Issue #10)](https://github.com/choptastic/sql_bridge/issues/10) (Thanks
  @th31nitiate!)
- The above work ensures that sql_bridge is now fully hex compliant and
  available in [hex.pm](https://hex.pm/packages/sql_bridge).
- Fix a big transaction bug in the mysql-otp bridge where transactions were
  mostly unreliable.
- Fix all dialyzer errors
- Added Github Actions testing

## 0.6.1 (2022-04-13)

- Added support for setting the hostname to a `{Mod,Fun}` tuple that will be
  called to determine which DB server to connect to.

## 0.6.0 (2022-03-14)

- Loosen the supervisor failure conditions, so `sql_bridge` doWorking Versione
  if the backend DB server goes offline for an extended period.
- Added `field_exists/2` to check for the existance of a field in a table

## 0.5.0 (2021-11-17)

- **BACKWARDS INCOMPATIBLE CHANGE**: Removed support for emysql
- Configured with rebar3

## 0.4.0 (2020-04-14)

- **BACKWARDS INCOMPATIBLE CHANGE**: Renamed `update` and `insert` to `qupdate`
  and `qinsert`.
- Created `update/[2,3]` and `insert/2` which are friendly maps to `plu` and
  `pli` respectively.
- Handling nested transactions and nested checkouts (just for mysql-otp right now)
- Give a warning message if query breaks in mysql-otp. Previously just returned
  `{error, Something}` but this generates an OTP warning as well.
- Fix atom encoding in epgsql
- Branched the module alias system into a new dependency module called `erlias`
- The built-in base64 converter now uses `b64fast`
- Add a timer-based logger. Calling `log_for_time(5)` will log all queries for
  5 seconds to `sql_bridge.log`

## 0.3.0 (2016-04-14)

- Normalize return values for date, time, timestamp, and decimal types.
- Fix `db:save` to properly decompose tables of the `database.tablename`
  format.
- Better normalization of `true`, `false` and NULLs to `undefined`
- Add more tests

## 0.2.0 (2016-03-10)

- Add record handling
- Improve stability in mysql-otp driver

## 0.1.1 (2016-01-22)

- Bugfix Release

## 0.1.0 (2016-01-21)

- Renamed SigmaSQL to SQL_Bridge
- Changed the core module from `db.erl` to `sql_bridge.erl`, and implemented
  the alias system.
- Add PostgreSQL support
- Add MySQL and PGSQL token replacement methods to be configurable. (`?` and
  `$1`, respectively)
- Add support for `mysql_otp` and `epgsql` drivers
- Add battery of tests

## 0.0.4 (2015-10-05)

- Unicode Fix
- Working with Erlang 18

## 0.0.2 (2014-08-14)

- Added Typespecs
- Bug Fixes
- Passing dialyzer

## 0.0.1 (2013-09-28)

- Supported Emysql driver only.
