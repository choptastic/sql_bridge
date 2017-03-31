## v0.3.1

  * Handling nested transactions and nested checkouts (just for mysql-otp right now)
  * Give a warning message if query breaks in mysql-otp. Previously just
    returned `{error, Something}` but this generates an OTP warning as well.
  * Fix atom encoding in epgsql

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
