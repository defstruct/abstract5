--
-- NOTE: Each command must be delimited with 2+ newlines.
--

CREATE TABLE page_entry (
    oid integer NOT NULL UNIQUE REFERENCES public.pobj(oid),
    name text,
    description text,
    status text NOT NULL,
    uri_path text NOT NULL,
    uri_filename text,
    reader text NOT NULL,
    evaluator text NOT NULL,
    printer text NOT NULL,
    pathname text,
    env text NOT NULL,
    area_template text,
    area_template_key text,
    parent_oid integer REFERENCES page_entry(oid),
    UNIQUE(uri_path, uri_filename)
);

CREATE TABLE block (
    oid integer NOT NULL UNIQUE REFERENCES public.pobj(oid),
    id text,
    name text,
    description text,
    content text NOT NULL,
    evaluator text NOT NULL,
    parent_oid integer REFERENCES page_entry(oid)
);

CREATE TABLE user (
    oid integer		NOT NULL UNIQUE REFERENCES public.pobj(oid),
    name		text NOT NULL,
    email		text NOT NULL,
    password		text NOT NULL,
    password_salt	text NOT NULL,
    active_p		boolean NOT NULL DEFAULT FALSE,
    validated_p		boolean NOT NULL DEFAULT FALSE,
    join_date		integer NOT NULL,
    avatar_p		boolean NOT NULL DEFAULT FALSE,
    last_online		integer NOT NULL,
    last_login		integer NOT NULL,
    prev_login		integer NOT NULL,
    num_logins		integer NOT NULL,
    person_oid		integer,
    group_oid		integer NOT NULL
);

CREATE TABLE address (
    addressable_oid	integer NOT NULL,
    street_number	text NOT NULL,
    street_name		text NOT NULL,
    surburb		text NOT NULL,
    state		text NOT NULL,
    post_code		text NOT NULL,
    country		text NOT NULL
);

CREATE INDEX IDX_address_addressable_oid on address(addressable_oid);

CREATE TABLE phone (
    callable_oid	integer NOT NULL,
    type		text NOT NULL,
    number		text NOT NULL
);

CREATE INDEX IDX_phone_callable_oid on phone(callable_oid);

CREATE TABLE person (
    oid integer		NOT NULL UNIQUE REFERENCES public.pobj(oid),
    name		text NOT NULL,
    description		text,
    primary_address_oid	integer,
    primary_phone_oid	integer
);

CREATE TABLE admin (
    oid integer NOT NULL UNIQUE REFERENCES public.pobj(oid),
    name text NOT NULL,
    site_oid integer
);

CREATE TABLE group (
    oid integer		NOT NULL UNIQUE REFERENCES public.pobj(oid),
    name		text NOT NULL,
    description		text,
    expiration_method	text,
    expiration_date	integer,
    expiration_action	text
);
