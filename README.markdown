# SigmaSQL

A SQL helper library, currently only running on top of
[emysql](https://github.com/Eonblast/Emysql), but with the intention of
eventually running on top a PostgreSQL driver as well.

## What it does

It provides a simple layer on top of existing SQL drivers, abstracting away the
pool management, and providing sensible default behaviors.  Further, it
provides a whole slew of methods for returning and updating data: lists,
tuples, proplists, dicts, existence checks, single fields returning and
setting, updating data from proplists, and more.

## Conventions

   * A process tracks which database it's connecting to, grabbing any of the
     available database pools. No need to identify a specific pool with
	 requests.
   * Connections are made automatically when the first query is attempted
   * A process determines which database to connect to via the `sigma_sql`
     application variable `lookup`.
   * This is used commonly to simplify the process of running a single codebase
     which connects to different database for different users or for different
	 host headers.

## Configuration

There is a sample config file in sample.config, but here are the configuration
settings currently available:

```erlang
[
    {sigma_sql, [
        {type, mysql},
        {host, "127.0.0.1"},
        {port, 3306},
        {user, "user"},
        {pass, "userpass"},
        %% There are three different ways to determine database
        %%
        %% 1) All requests go to a single database:
        {lookup, database_name}
        %%
        %% 2) Before a request is made, run a lookup function in another
        %%     module to determine which database:
        {lookup, {lookup_module, lookup_function}}
        %%
        %% 3) Before a request is made, run an anonymous lookup function:
        {lookup, fun() -> returns_an_atom_of_db_name() end}
    ]}
].
```

The most complicated us the `lookup` application variable. Lookup can be one of
three different kinds of values:

  * Atom: That's the database every request will use.
  * {Module, Function}: Call Module:Function() to determine which database to
    connect to.
  * Function (arity 1): Call Function() to determine which database to connect
    to.

## API

### Conventions

#### Abbreviations

Due to my obsession with brevity, almost all function calls are acronyms of
something.  Learning those conventions will save you keystrokes, minimize the
chance for typos, and shorten your code.  The drawback is that it's not
entirely obvious on a cursory glance what a function returns (for example:
`db:fffr` is not exactly obvious that it stands for "(F)irst (F)ield of (F)irst
(R)ecord").

But you'll learn common shortcuts:

`q` -> query
`t` -> tuple
`d` -> dict
`l` -> list
`pl` -> proplist
`ff` -> first field
`fr` -> first record
`i` -> insert
`u` -> update

#### Prepared Statements?

SigmaSQL currently does not offer prepared statements, but will do safe
variable replacement using a similar convention, replacing `?` in order.

For example:

```erlang
db:q("Select * from login where username=? or email=?", [Loginid, Email])`
```

#### Singular Table Names

I know it's common for database designers to name their tables with the plural
form of a noun to indicate that it's a collection of things (e.g. "logins"
instead of "login"), while still using the singular as the name of key fields
(so a table of logins would be called "logins", but the key would be
"loginid").

Well, despite english being my native language, I find it to be a terribly,
horribly inconsistent language, and refuse to try to make code figure out if
the plural of "child" is "children" or "childs". As such, SigmaSQL makes a lot
of assumptions that your codebase will use a singular table name, and that the
key of that table is named `Tablename ++ "id"`.

Using the example above, my table of logins would be called "login" and the
primary key is "loginid".

#### Insert or Update Determination

There are some helper functions in SigmaSQL that will attempt to determine if
we're updating or inserting a new one.  The basic rule is this: If the key
field specified has a value of `0` or `undefined`, it will be an insert,
assuming the database will do the auto increment for us. If it's anything else,
it's an update.

### Functions

#### Select Queries

#### Insert, Update, Delete Queries

## About

Copyright (c) 2013 [Jesse Gumm](http://sigma-star.com/page/jesse)
([@jessegumm](http://twitter.com/jessegumm))

MIT License
