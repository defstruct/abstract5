--
-- NOTE: Each command must be delimited with 2+ newlines.
--
SET client_encoding = 'UTF8';

SET standard_conforming_strings = on;

SET client_min_messages = warning;

CREATE PROCEDURAL LANGUAGE plpgsql;

CREATE FUNCTION new_schema_and_get_prev_schema(text) RETURNS text
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      the_new_schema alias for $1;
      the_old_schema TEXT;
BEGIN
	select current_schema() into the_old_schema;
	execute	'set search_path to ' || the_new_schema;
        RETURN the_old_schema;
END;$_$;

CREATE FUNCTION insert_pobj(text, text, text, text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      the_schema alias for $1;
      the_class alias for $2;
      the_table alias for $3;
      the_attributes alias for $4;
      the_values alias for $5;
      the_pobj_oid integer;
BEGIN
	select nextval('public.oid_seq'::regclass) into the_pobj_oid;
        insert into public.pobj (oid, schema, class) values (the_pobj_oid, the_schema, the_class);
        execute 'insert into ' || the_table || the_attributes || 'values' || the_values || the_pobj_oid || ')';
        RETURN the_pobj_oid;
END;$_$;

CREATE SEQUENCE oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE pobj (
    oid integer NOT NULL PRIMARY KEY,
    schema text NOT NULL,
    class text NOT NULL
);

CREATE TABLE admin (
    oid integer NOT NULL UNIQUE REFERENCES pobj(oid),
    name text NOT NULL,
    site_oid integer
);

CREATE TABLE site (
    oid integer NOT NULL UNIQUE REFERENCES pobj(oid),
    name text NOT NULL UNIQUE,
    description text,
    home_folder text,
    locale text,
    db_schema text NOT NULL
);

CREATE TABLE subdomain (
    oid integer NOT NULL UNIQUE REFERENCES pobj(oid),
    name text NOT NULL UNIQUE,
    site_oid integer NOT NULL REFERENCES site(oid)
);

CREATE TABLE page_entry (
    oid integer NOT NULL UNIQUE REFERENCES pobj(oid),
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
    parent_oid integer REFERENCES page_entry(oid),
    UNIQUE(uri_path, uri_filename)
);


CREATE TABLE block (
    oid integer NOT NULL UNIQUE REFERENCES pobj(oid),
    name text,
    description text,
    content text NOT NULL,
    evaluator text NOT NULL,
    parent_oid int8 REFERENCES page_entry(oid)
);