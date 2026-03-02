use sql_bridge_test;
alter table other add u uuid;

drop table if exists other_uuid;
create table other_uuid (
    id uuid primary key,
    some_text text
);
