## 0.7.0 (in development)

* The mysql-otp bridge has been reworked to use the mainline
  [mysql-otp](https://github.com/mysql-otp/mysql-otp) repo (rather than
  @choptastic's fork).  This requires
  [new functionality](https://github.com/mysql-otp/mysql-otp/pull/194) in
  mysql-otp.
* The epgsql bridge now relies on mainline
  [epgsql](https://github.com/epgsql/epgsql) (rather than @choptastic's
  fork). This ended up requiring migrating @choptastic's epgsql customizations
  to become epgsql [custom
  codecs](https://github.com/epgsql/epgsql/blob/devel/doc/pluggable_types.md)
  and [epgsql_decimal](https://github.com/egobrain/epgsql_decimal).
* Fixed `epgsql`'s decoding of `decimal`/`numeric` types to return a float or
  integer. (Fixes [Issue #5](https://github.com/choptastic/sql_bridge/issues/5))
* Fixed a crash in the `epgsql` driver
  ([Issue #10)](https://github.com/choptastic/sql_bridge/issues/10) (Thanks
  @th31nitiate!)
* The above work ensures that sql_bridge is now fully hex compliant and
  available in [hex.pm](https://hex.pm/packages/sql_bridge).
* Fix a big transaction bug in the mysql-otp bridge where transactions were
  mostly unreliable.
* Fix all dialyzer errors
* Added Github CI testing

## 0.6.1

* Added support for setting the hostname to a `{Mod,Fun}` tuple that will be
  called to determine which DB server to connect to.

## 0.6.0

* Loosen the supervisor failure conditions, so `sql_bridge` doesn't go offline
  if the backend DB server goes offline for an extended period.

## 0.5.1

* Added `field_exists/2` to check for the existance of a field in a table

## 0.5.0

* **BACKWARDS INCOMPATIBLE CHANGE**: Removed support for emysql
* Configured with rebar3

## v0.4.0

* **BACKWARDS INCOMPATIBLE CHANGE**: Renamed `update` and `insert` to `qupdate`
  and `qinsert`.
* Created `update/[2,3]` and `insert/2` which are friendly maps to `plu` and
  `pli` respectively.
* Handling nested transactions and nested checkouts (just for mysql-otp right now)
* Give a warning message if query breaks in mysql-otp. Previously just returned
  `{error, Something}` but this generates an OTP warning as well.
* Fix atom encoding in epgsql
* Branched the module alias system into a new dependency module called `erlias`
* The built-in base64 converter now uses `b64fast`
* Add a timer-based logger. Calling `log_for_time(5)` will log all queries for
  5 seconds to `sql_bridge.log`

## v0.3.0

* Normalize return values for date, time, timestamp, and decimal types.
* Fix `db:save` to properly decompose tables of the `database.tablename`
  format.
* Better normalization of `true`, `false` and NULLs to `undefined`
* Add more tests

## v0.2.0

* Add record handling
* Improve stability in mysql-otp driver

## v0.1.0

* Add support for mysql-otp and epgsql drivers
* Add battery of tests

## Prior Versions
* Supported Emysql driver only.
