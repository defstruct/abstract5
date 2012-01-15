--
-- NOTE: Each command must be delimited with 2+ newlines.
--
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

CREATE SEQUENCE oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE repl_entry (
    oid integer DEFAULT nextval('oid_seq'::regclass) NOT NULL PRIMARY KEY,
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
    parent_oid integer REFERENCES repl_entry(oid),
    UNIQUE(uri_path, uri_filename)
);
