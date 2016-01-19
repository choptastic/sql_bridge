create table fruit (
	fruitid serial primary key,
	fruit varchar(50),
	description text,
	quantity integer,
	picture bytea
);
