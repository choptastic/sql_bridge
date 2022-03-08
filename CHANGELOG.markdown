## 0.5.1
  * Added `field_exists/2` to check for the existance of a field in a table

## 0.5.0
  * **BACKWARDS INCOMPATIBLE CHANGE**: Removed support for emysql
  * Configured with rebar3

## v0.4.0
  * **BACKWARDS INCOMPATIBLE CHANGE**: Renamed `update` and `insert` to `qupdate` and `qinsert`.
  * Created `update/[2,3]` and `insert/2` which are friendly maps to `plu` and `pli` respectively.
  * Handling nested transactions and nested checkouts (just for mysql-otp right now)
  * Give a warning message if query breaks in mysql-otp. Previously just
    returned `{error, Something}` but this generates an OTP warning as well.
  * Fix atom encoding in epgsql
  * Branched the module alias system into a new dependency module called `erlias`
  * The built-in base64 converter now uses `b64fast`
  * Add a timer-based logger. Calling `log_for_time(5)` will log all queries for 5 seconds to `sql_bridge.log`

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
