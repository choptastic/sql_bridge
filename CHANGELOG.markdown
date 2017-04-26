## v0.4.0 (in progress)
  * **BACKWARDS INCOMPATIBLE CHANGE**: Renamed `update` and `insert` to `qupdate` and `qinsert`.
  * Created a new `update/[2,3]` and `insert/2` to which are friendly maps to
	`plu` and `pli` respectively.
  * Fix atom encoding in epgsql.
  * Better error messages for mysql-otp
  * Nested transactions working better

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
